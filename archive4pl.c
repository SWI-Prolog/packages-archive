/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012-2014, VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#ifdef __WINDOWS__
#define LIBARCHIVE_STATIC 1
#endif
#include <archive.h>
#include <archive_entry.h>
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#else					/* assume complete recent libarchive */
#define HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_BZIP2 1
#define HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_COMPRESS 1
#define HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_GZIP 1
#define HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_LZMA 1
#define HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_NONE 1
#define HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_XZ 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_AR 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_CPIO 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_EMPTY 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_ISO9660 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_MTREE 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_RAW 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_TAR 1
#define HAVE_ARCHIVE_READ_SUPPORT_FORMAT_ZIP 1
#define HAVE_ARCHIVE_READ_CLOSE 1
#endif

#ifndef HAVE_ARCHIVE_READ_CLOSE
#define archive_read_close(ar) archive_read_finish(ar)
#endif

#if ARCHIVE_VERSION_NUMBER < 3000000
#define archive_read_support_filter_all archive_read_support_compression_all

#ifdef HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_BZIP2
#define HAVE_ARCHIVE_READ_SUPPORT_FILTER_BZIP2 1
#define archive_read_support_filter_bzip2 archive_read_support_compression_bzip2
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_COMPRESS
#define HAVE_ARCHIVE_READ_SUPPORT_FILTER_COMPRESS
#define archive_read_support_filter_compress archive_read_support_compression_compress
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_GZIP
#define HAVE_ARCHIVE_READ_SUPPORT_FILTER_GZIP
#define archive_read_support_filter_gzip archive_read_support_compression_gzip
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_LZMA
#define HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZMA
#define archive_read_support_filter_lzma archive_read_support_compression_lzma
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_NONE
#define HAVE_ARCHIVE_READ_SUPPORT_FILTER_NONE
#define archive_read_support_filter_none archive_read_support_compression_none
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_COMPRESSION_XZ
#define HAVE_ARCHIVE_READ_SUPPORT_FILTER_XZ
#define archive_read_support_filter_xz archive_read_support_compression_xz
#endif
#endif /*ARCHIVE_VERSION_NUMBER < 3000000*/


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		Design of the libarchive interface

An archive is represented by a   symbol (blob). The C-structure contains
the archive, the current header and some state information.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define ARCHIVE_MAGIC 348184378

typedef enum ar_status
{ AR_VIRGIN = 0,
  AR_OPENED,
  AR_NEW_ENTRY,
  AR_OPENED_ENTRY,
  AR_CLOSED_ENTRY
} ar_status;

typedef struct archive_wrapper
{ atom_t		symbol;		/* Associated symbol */
  IOSTREAM *		data;		/* Input data */
  unsigned int		type;		/* Type of format supported */
  int			magic;		/* magic code */
  ar_status		status;		/* Current status */
  int			close_parent;	/* Close the parent handle */
  int			closed_archive;	/* Archive was closed with open entry */
  struct archive *	archive;	/* Actual archive handle */
  struct archive_entry *entry;		/* Current entry */
} archive_wrapper;

static void free_archive(archive_wrapper *ar);


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static atom_t ATOM_close_parent;
static atom_t ATOM_compression;
static atom_t ATOM_filter;
static atom_t ATOM_format;
static atom_t ATOM_all;
static atom_t ATOM_bzip2;
static atom_t ATOM_compress;
static atom_t ATOM_gzip;
static atom_t ATOM_grzip;
static atom_t ATOM_lrzip;
static atom_t ATOM_lzip;
static atom_t ATOM_lzma;
static atom_t ATOM_lzop;
static atom_t ATOM_none;
static atom_t ATOM_rpm;
static atom_t ATOM_uu;
static atom_t ATOM_xz;
static atom_t ATOM_7zip;
static atom_t ATOM_ar;
static atom_t ATOM_cab;
static atom_t ATOM_cpio;
static atom_t ATOM_empty;
static atom_t ATOM_gnutar;
static atom_t ATOM_iso9960;
static atom_t ATOM_lha;
static atom_t ATOM_mtree;
static atom_t ATOM_rar;
static atom_t ATOM_raw;
static atom_t ATOM_tar;
static atom_t ATOM_xar;
static atom_t ATOM_zip;
static atom_t ATOM_file;
static atom_t ATOM_link;
static atom_t ATOM_socket;
static atom_t ATOM_character_device;
static atom_t ATOM_block_device;
static atom_t ATOM_directory;
static atom_t ATOM_fifo;

static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_archive_error2;
static functor_t FUNCTOR_filetype1;
static functor_t FUNCTOR_format1;
static functor_t FUNCTOR_mtime1;
static functor_t FUNCTOR_size1;
static functor_t FUNCTOR_link_target1;

		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/

static void
acquire_archive(atom_t symbol)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
  ar->symbol = symbol;
}


static int
release_archive(atom_t symbol)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);
  struct archive *a;

  assert(ar->status != AR_OPENED_ENTRY);

  if ( (a=ar->archive) )
  { ar->archive = NULL;
    archive_read_close(a);
  }

  free_archive(ar);
  PL_free(ar);

  return TRUE;
}

static int
compare_archives(atom_t a, atom_t b)
{ archive_wrapper *ara = PL_blob_data(a, NULL, NULL);
  archive_wrapper *arb = PL_blob_data(b, NULL, NULL);

  return ( ara > arb ?  1 :
	   ara < arb ? -1 : 0
	 );
}

static int
write_archive(IOSTREAM *s, atom_t symbol, int flags)
{ archive_wrapper *ar = PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<archive>(%p)", ar);

  return TRUE;
}

static PL_blob_t archive_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "archive",
  release_archive,
  compare_archives,
  write_archive,
  acquire_archive
};


static int
get_archive(term_t t, archive_wrapper **arp)
{ PL_blob_t *type;
  void *data;

  if ( PL_get_blob(t, &data, NULL, &type) && type == &archive_blob)
  { archive_wrapper *ar = data;

    assert(ar->magic == ARCHIVE_MAGIC);

    if ( ar->symbol )
    { *arp = ar;

      return TRUE;
    }

    PL_permission_error("access", "closed_archive", t);
    return FALSE;
  }

  return PL_type_error("archive", t);
}



		 /*******************************
		 *	      CALLBACKS		*
		 *******************************/

static int
ar_open(struct archive *a, void *cdata)
{ return ARCHIVE_OK;
}

static int
ar_close(struct archive *a, void *cdata)
{ archive_wrapper *ar = cdata;

  if ( ar->close_parent )
  { if ( Sclose(ar->data) != 0 )
    { archive_set_error(ar->archive, errno, "Close failed");
      ar->data = NULL;
      return ARCHIVE_FATAL;
    }
    ar->data = NULL;
  }

  return ARCHIVE_OK;				/* TBD: close_parent */
}

static ssize_t
ar_read(struct archive *a, void *cdata, const void **buffer)
{ archive_wrapper *ar = cdata;

  if ( Sfeof(ar->data) )
  { return 0;
  } else
  { ssize_t bytes = ar->data->limitp - ar->data->bufp;

    *buffer = ar->data->bufp;
    ar->data->bufp = ar->data->limitp;
    ar->data->position->byteno += bytes;

    return bytes;
  }
}

#ifndef __LA_INT64_T
#define __LA_INT64_T off_t
#endif

static __LA_INT64_T
ar_skip(struct archive *a, void *cdata, __LA_INT64_T request)
{ archive_wrapper *ar = cdata;

  if ( Sseek64(ar->data, request, SIO_SEEK_CUR) == 0 )
    return request;
  Sclearerr(ar->data);

  return 0;				/* cannot skip; library will read */
}



		 /*******************************
		 *	      PROLOG		*
		 *******************************/

static void
free_archive(archive_wrapper *ar)
{
}

static int
archive_error(archive_wrapper *ar)
{ int eno = archive_errno(ar->archive);

  if ( eno != 0 )
  { const char *s = archive_error_string(ar->archive);
    term_t ex = PL_new_term_ref();

    if ( PL_unify_term(ex,
		       PL_FUNCTOR, FUNCTOR_error2,
		         PL_FUNCTOR, FUNCTOR_archive_error2,
		           PL_INT, errno,
		           PL_CHARS, s,
		         PL_VARIABLE) )
      return PL_raise_exception(ex);

    return FALSE;
  }

  return TRUE;
}


#define	FILTER_ALL	  0x0000ffff
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_BZIP2
#define	FILTER_BZIP2	  0x00000001
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_COMPRESS
#define	FILTER_COMPRESS 0x00000002
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_GZIP
#define	FILTER_GZIP	  0x00000004
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_GRZIP
#define	FILTER_GRZIP	  0x00000008
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LRZIP
#define	FILTER_LRZIP	  0x00000010
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZIP
#define	FILTER_LZIP	  0x00000020
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZMA
#define	FILTER_LZMA	  0x00000040
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_LZOP
#define	FILTER_LZOP	  0x00000080
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_NONE
#define	FILTER_NONE	  0x00000100
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_RPM
#define	FILTER_RPM	  0x00000200
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_UU
#define	FILTER_UU	  0x00000400
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FILTER_XZ
#define	FILTER_XZ	  0x00000800
#endif

#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_7ZIP
#define FORMAT_7ZIP	  0x00010000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_AR
#define FORMAT_AR	  0x00020000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_CAB
#define FORMAT_CAB	  0x00040000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_CPIO
#define FORMAT_CPIO	  0x00080000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_EMPTY
#define FORMAT_EMPTY	  0x00100000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_GNUTAR
#define FORMAT_GNUTAR	  0x00200000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_ISO9660
#define FORMAT_ISO9960	  0x00400000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_LHA
#define FORMAT_LHA	  0x00800000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_MTREE
#define FORMAT_MTREE	  0x01000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_RAR
#define FORMAT_RAR	  0x02000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_RAW
#define FORMAT_RAW	  0x04000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_TAR
#define FORMAT_TAR	  0x08000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_XAR
#define FORMAT_XAR	  0x10000000
#endif
#ifdef HAVE_ARCHIVE_READ_SUPPORT_FORMAT_ZIP
#define FORMAT_ZIP	  0x20000000
#endif

#define FORMAT_MASK	  0xffff0000
#define FORMAT_ALL	  (FORMAT_MASK&~FORMAT_RAW)

static void
enable_type(archive_wrapper *ar, int type,
	    int (*f)(struct archive *ar))
{ if ( (ar->type & type) )
  { if ( (*f)(ar->archive) != ARCHIVE_OK )
      ar->type &= ~type;
  }
}

static foreign_t
archive_open_stream(term_t data, term_t handle, term_t options)
{ IOSTREAM *datas;
  archive_wrapper *ar;
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t arg  = PL_new_term_ref();

  if ( !PL_get_stream_handle(data, &datas) )
    return FALSE;
  if ( !(datas->flags & SIO_INPUT) )
  { PL_release_stream(datas);
    return PL_domain_error("input_stream", data);
  }

  ar = PL_malloc(sizeof(*ar));
  memset(ar, 0, sizeof(*ar));
  ar->data = datas;
  ar->magic = ARCHIVE_MAGIC;
  if ( !PL_unify_blob(handle, ar, sizeof(*ar), &archive_blob) )
    return FALSE;

  while( PL_get_list_ex(tail, head, tail) )
  { atom_t name;
    int arity;

    if ( !PL_get_name_arity(head, &name, &arity) ||
	 !PL_get_arg(1, head, arg) )
      return PL_type_error("option", head);
    if ( name == ATOM_compression || name == ATOM_filter )
    { atom_t c;

      if ( !PL_get_atom_ex(arg, &c) )
	return FALSE;

      if ( c == ATOM_all )
	ar->type |= FILTER_ALL;
#ifdef FILTER_BZIP2
      else if ( c == ATOM_bzip2 )
	ar->type |= FILTER_BZIP2;
#endif
#ifdef FILTER_COMPRESS
      else if ( c == ATOM_compress )
	ar->type |= FILTER_COMPRESS;
#endif
#ifdef FILTER_GZIP
      else if ( c == ATOM_gzip )
	ar->type |= FILTER_GZIP;
#endif
#ifdef FILTER_GRZIP
      else if ( c == ATOM_grzip )
	ar->type |= FILTER_GRZIP;
#endif
#ifdef FILTER_LRZIP
      else if ( c == ATOM_lrzip )
	ar->type |= FILTER_LRZIP;
#endif
#ifdef FILTER_LZIP
      else if ( c == ATOM_lzip )
	ar->type |= FILTER_LZIP;
#endif
#ifdef FILTER_LZMA
      else if ( c == ATOM_lzma )
	ar->type |= FILTER_LZMA;
#endif
#ifdef FILTER_LZOP
      else if ( c == ATOM_lzop )
	ar->type |= FILTER_LZOP;
#endif
#ifdef FILTER_NONE
      else if ( c == ATOM_none )
	ar->type |= FILTER_NONE;
#endif
#ifdef FILTER_RPM
      else if ( c == ATOM_rpm )
	ar->type |= FILTER_RPM;
#endif
#ifdef FILTER_UU
      else if ( c == ATOM_uu )
	ar->type |= FILTER_UU;
#endif
#ifdef FILTER_XZ
      else if ( c == ATOM_xz )
	ar->type |= FILTER_XZ;
#endif
      else
	return PL_domain_error("filter", arg);
    } else if ( name == ATOM_format )
    { atom_t f;

      if ( !PL_get_atom_ex(arg, &f) )
	return FALSE;

      if ( f == ATOM_all )
	ar->type |= FORMAT_ALL;
#ifdef FORMAT_7ZIP
      else if ( f == ATOM_7zip )
	ar->type |= FORMAT_7ZIP;
#endif
#ifdef FORMAT_AR
      else if ( f == ATOM_ar )
	ar->type |= FORMAT_AR;
#endif
#ifdef FORMAT_CAB
      else if ( f == ATOM_cab )
	ar->type |= FORMAT_CAB;
#endif
#ifdef FORMAT_CPIO
      else if ( f == ATOM_cpio )
	ar->type |= FORMAT_CPIO;
#endif
#ifdef FORMAT_EMPTY
      else if ( f == ATOM_empty )
	ar->type |= FORMAT_EMPTY;
#endif
#ifdef FORMAT_GNUTAR
      else if ( f == ATOM_gnutar )
	ar->type |= FORMAT_GNUTAR;
#endif
#ifdef FORMAT_ISO9960
      else if ( f == ATOM_iso9960 )
	ar->type |= FORMAT_ISO9960;
#endif
#ifdef FORMAT_LHA
      else if ( f == ATOM_lha )
	ar->type |= FORMAT_LHA;
#endif
#ifdef FORMAT_MTREE
      else if ( f == ATOM_mtree )
	ar->type |= FORMAT_MTREE;
#endif
#ifdef FORMAT_RAR
      else if ( f == ATOM_rar )
	ar->type |= FORMAT_RAR;
#endif
#ifdef FORMAT_RAW
      else if ( f == ATOM_raw )
	ar->type |= FORMAT_RAW;
#endif
#ifdef FORMAT_TAR
      else if ( f == ATOM_tar )
	ar->type |= FORMAT_TAR;
#endif
#ifdef FORMAT_XAR
      else if ( f == ATOM_xar )
	ar->type |= FORMAT_XAR;
#endif
#ifdef FORMAT_ZIP
      else if ( f == ATOM_zip )
	ar->type |= FORMAT_ZIP;
#endif
      else
	return PL_domain_error("format", arg);
    } else if ( name == ATOM_close_parent )
    { if ( !PL_get_bool_ex(arg, &ar->close_parent) )
	return FALSE;
    }
  }
  if ( !PL_get_nil_ex(tail) )
    return FALSE;

  if ( !(ar->type & FILTER_ALL) )
    ar->type |= FILTER_ALL;
  if ( !(ar->type & FORMAT_MASK) )
    ar->type |= FORMAT_ALL;

  if ( !(ar->archive = archive_read_new()) )
    return PL_resource_error("memory");

  if ( (ar->type & FILTER_ALL) == FILTER_ALL )
  { archive_read_support_filter_all(ar->archive);
  } else
  {
#ifdef FILTER_BZIP2
    enable_type(ar, FILTER_BZIP2,    archive_read_support_filter_bzip2);
#endif
#ifdef FILTER_COMPRESS
    enable_type(ar, FILTER_COMPRESS, archive_read_support_filter_compress);
#endif
#ifdef FILTER_GZIP
    enable_type(ar, FILTER_GZIP,     archive_read_support_filter_gzip);
#endif
#ifdef FILTER_GRZIP
    enable_type(ar, FILTER_GRZIP,     archive_read_support_filter_grzip);
#endif
#ifdef FILTER_LRZIP
    enable_type(ar, FILTER_LRZIP,     archive_read_support_filter_lrzip);
#endif
#ifdef FILTER_LZIP
    enable_type(ar, FILTER_LZIP,     archive_read_support_filter_lzip);
#endif
#ifdef FILTER_LZMA
    enable_type(ar, FILTER_LZMA,     archive_read_support_filter_lzma);
#endif
#ifdef FILTER_LZOP
    enable_type(ar, FILTER_LZOP,     archive_read_support_filter_lzop);
#endif
#ifdef FILTER_NONE
    enable_type(ar, FILTER_NONE,     archive_read_support_filter_none);
#endif
#ifdef FILTER_RPM
    enable_type(ar, FILTER_RPM,      archive_read_support_filter_rpm);
#endif
#ifdef FILTER_UU
    enable_type(ar, FILTER_UU,       archive_read_support_filter_uu);
#endif
#ifdef FILTER_XZ
    enable_type(ar, FILTER_XZ,       archive_read_support_filter_xz);
#endif
  }

  if ( (ar->type & FORMAT_ALL) == FORMAT_ALL )
  { archive_read_support_format_all(ar->archive);
#ifdef FORMAT_RAW
    enable_type(ar, FORMAT_RAW,     archive_read_support_format_raw);
#endif
  } else
  {
#ifdef FORMAT_7ZIP
    enable_type(ar, FORMAT_7ZIP,    archive_read_support_format_7zip);
#endif
#ifdef FORMAT_AR
    enable_type(ar, FORMAT_AR,      archive_read_support_format_ar);
#endif
#ifdef FORMAT_CAB
    enable_type(ar, FORMAT_CAB,     archive_read_support_format_cab);
#endif
#ifdef FORMAT_CPIO
    enable_type(ar, FORMAT_CPIO,    archive_read_support_format_cpio);
#endif
#ifdef FORMAT_EMPTY
    enable_type(ar, FORMAT_EMPTY,   archive_read_support_format_empty);
#endif
#ifdef FORMAT_GNUTAR
    enable_type(ar, FORMAT_GNUTAR,  archive_read_support_format_gnutar);
#endif
#ifdef FORMAT_ISO9960
    enable_type(ar, FORMAT_ISO9960, archive_read_support_format_iso9660);
#endif
#ifdef FORMAT_LHA
    enable_type(ar, FORMAT_LHA,     archive_read_support_format_lha);
#endif
#ifdef FORMAT_MTREE
    enable_type(ar, FORMAT_MTREE,   archive_read_support_format_mtree);
#endif
#ifdef FORMAT_RAR
    enable_type(ar, FORMAT_RAR,     archive_read_support_format_rar);
#endif
#ifdef FORMAT_RAW
    enable_type(ar, FORMAT_RAW,     archive_read_support_format_raw);
#endif
#ifdef FORMAT_TAR
    enable_type(ar, FORMAT_TAR,     archive_read_support_format_tar);
#endif
#ifdef FORMAT_XAR
    enable_type(ar, FORMAT_XAR,     archive_read_support_format_xar);
#endif
#ifdef FORMAT_ZIP
    enable_type(ar, FORMAT_ZIP,     archive_read_support_format_zip);
#endif
  }

  if ( archive_read_open2(ar->archive, ar,
			  ar_open, ar_read, ar_skip, ar_close) == ARCHIVE_OK )

  { ar->status = AR_OPENED;
    return TRUE;
  }

  return archive_error(ar);
}


static foreign_t
archive_property(term_t archive, term_t prop, term_t value)
{ archive_wrapper *ar;
  atom_t pn;
  const char *s;

  if ( !get_archive(archive, &ar) ||
       !PL_get_atom_ex(prop, &pn) )
    return FALSE;

#ifdef HAVE_ARCHIVE_FILTER_COUNT
  if ( pn == ATOM_filter )
  { int i, fcount = archive_filter_count(ar->archive);
    term_t tail = PL_copy_term_ref(value);
    term_t head = PL_new_term_ref();

    for(i=0; i<fcount; i++)
    { s = archive_filter_name(ar->archive, i);

      if ( !s || strcmp(s, "none") == 0 )
	continue;

      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify_atom_chars(head, s) )
	return FALSE;
    }
    return PL_unify_nil(tail);
  }
#endif

  return FALSE;
}


static foreign_t
archive_next_header(term_t archive, term_t name)
{ archive_wrapper *ar;
  int rc;

  if ( !get_archive(archive, &ar) )
    return FALSE;
  if ( ar->status == AR_NEW_ENTRY )
    archive_read_data_skip(ar->archive);

  while ( (rc=archive_read_next_header(ar->archive, &ar->entry)) == ARCHIVE_OK )
  { if ( PL_unify_wchars(name, PL_ATOM, -1,
			 archive_entry_pathname_w(ar->entry)) )
    { ar->status = AR_NEW_ENTRY;
      return TRUE;
    }
    if ( PL_exception(0) )
      return FALSE;
  }

  if ( rc == ARCHIVE_EOF )
    return FALSE;			/* simply at the end */

  return archive_error(ar);
}


static foreign_t
archive_close(term_t archive)
{ archive_wrapper *ar;
  int rc;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( ar->status == AR_OPENED_ENTRY )
  { ar->closed_archive = TRUE;

    return TRUE;
  } else if ( (rc=archive_read_close(ar->archive)) == ARCHIVE_OK )
  { ar->entry = NULL;
    ar->archive = NULL;
    ar->symbol = 0;

    return TRUE;
  }

  return archive_error(ar);
}

		 /*******************************
		 *	       HEADERS		*
		 *******************************/

static foreign_t
archive_header_prop(term_t archive, term_t field)
{ archive_wrapper *ar;
  functor_t prop;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( !PL_get_functor(field, &prop) )
    return PL_type_error("compound", field);
  if ( ar->status != AR_NEW_ENTRY )
    return PL_permission_error("access", "archive_entry", archive);

  if ( prop == FUNCTOR_filetype1 )
  { __LA_MODE_T type = archive_entry_filetype(ar->entry);
    atom_t name;
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    switch(type&AE_IFMT)
    { case AE_IFREG:  name = ATOM_file;             break;
      case AE_IFLNK:  name = ATOM_link;             break;
      case AE_IFSOCK: name = ATOM_socket;           break;
      case AE_IFCHR:  name = ATOM_character_device; break;
      case AE_IFBLK:  name = ATOM_block_device;     break;
      case AE_IFDIR:  name = ATOM_directory;        break;
      case AE_IFIFO:  name = ATOM_fifo;             break;
      default:
	assert(0);
    }
    return PL_unify_atom(arg, name);
  } else if ( prop == FUNCTOR_mtime1 )
  { time_t stamp = archive_entry_mtime(ar->entry);
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    return PL_unify_float(arg, (double)stamp);
  } else if ( prop == FUNCTOR_size1 )
  { int64_t size = archive_entry_size(ar->entry);
    term_t arg = PL_new_term_ref();
    _PL_get_arg(1, field, arg);

    return PL_unify_int64(arg, size);
  } else if ( prop == FUNCTOR_link_target1 )
  { __LA_MODE_T type = archive_entry_filetype(ar->entry);
    const wchar_t *target = NULL;

    switch(type&AE_IFMT)
    { case AE_IFLNK:
	target = archive_entry_symlink_w(ar->entry);
        break;
    }

    if ( target )
    { term_t arg = PL_new_term_ref();
      _PL_get_arg(1, field, arg);

      return PL_unify_wchars(arg, PL_ATOM, (size_t)-1, target);
    }

    return FALSE;
  } else if ( prop == FUNCTOR_format1 )
  { const char *s = archive_format_name(ar->archive);

    if ( s )
    { char lwr[50];
      char *o;
      term_t arg = PL_new_term_ref();
      _PL_get_arg(1, field, arg);

      for(o=lwr; *s && o < lwr+sizeof(lwr); )
	*o++ = tolower(*s++);

      *o = '\0';

      return PL_unify_atom_chars(arg, lwr);
    }
  }

  return PL_domain_error("archive_header_property", field);
}


		 /*******************************
		 *	    READ MEMBERS	*
		 *******************************/

static ssize_t
ar_read_entry(void *handle, char *buf, size_t size)
{ archive_wrapper *ar = handle;

  return archive_read_data(ar->archive, buf, size);
}

static int
ar_close_entry(void *handle)
{ archive_wrapper *ar = handle;

  if ( ar->status == AR_OPENED_ENTRY )
  { PL_unregister_atom(ar->symbol);
    ar->status = AR_CLOSED_ENTRY;
  }
  if ( ar->closed_archive )
  { archive_read_close(ar->archive);
    ar->entry = NULL;
    ar->archive = NULL;
    ar->symbol = 0;
  }

  return 0;
}

static int
ar_control_entry(void *handle, int op, void *data)
{ archive_wrapper *ar = handle;

  (void)ar;

  switch(op)
  { case SIO_SETENCODING:
      return 0;				/* allow switching encoding */
    case SIO_GETSIZE:
      *((int64_t*)data) = archive_entry_size(ar->entry);
      return 0;
    default:
      return -1;
  }
}

static IOFUNCTIONS ar_entry_functions =
{ ar_read_entry,
  NULL,
  NULL,					/* seek */
  ar_close_entry,
  ar_control_entry,			/* control */
  NULL,					/* seek64 */
};

static foreign_t
archive_open_entry(term_t archive, term_t stream)
{ archive_wrapper *ar;
  IOSTREAM *s;

  if ( !get_archive(archive, &ar) )
    return FALSE;

  if ( (s=Snew(ar, SIO_INPUT|SIO_RECORDPOS, &ar_entry_functions)) )
  { ar->status = AR_OPENED_ENTRY;
    if ( PL_unify_stream(stream, s) )
    { PL_register_atom(ar->symbol);	/* We may no longer reference the */
      return TRUE;			/* archive itself */
    }
    Sclose(s);
    return FALSE;
  }

  return PL_resource_error("memory");
}


		 /*******************************
		 *	      INSTALL		*
		 *******************************/

#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)

install_t
install_archive4pl(void)
{ MKATOM(close_parent);
  MKATOM(compression);
  MKATOM(filter);
  MKATOM(format);
  MKATOM(all);
  MKATOM(bzip2);
  MKATOM(compress);
  MKATOM(gzip);
  MKATOM(grzip);
  MKATOM(lrzip);
  MKATOM(lzip);
  MKATOM(lzma);
  MKATOM(lzop);
  MKATOM(none);
  MKATOM(rpm);
  MKATOM(uu);
  MKATOM(xz);
  ATOM_7zip = PL_new_atom("7zip");
  MKATOM(ar);
  MKATOM(cab);
  MKATOM(cpio);
  MKATOM(empty);
  MKATOM(gnutar);
  MKATOM(iso9960);
  MKATOM(lha);
  MKATOM(mtree);
  MKATOM(rar);
  MKATOM(raw);
  MKATOM(tar);
  MKATOM(xar);
  MKATOM(zip);
  MKATOM(file);
  MKATOM(link);
  MKATOM(socket);
  MKATOM(character_device);
  MKATOM(block_device);
  MKATOM(directory);
  MKATOM(fifo);

  MKFUNCTOR(error,         2);
  MKFUNCTOR(archive_error, 2);
  MKFUNCTOR(filetype,      1);
  MKFUNCTOR(mtime,         1);
  MKFUNCTOR(size,          1);
  MKFUNCTOR(link_target,   1);
  MKFUNCTOR(format,        1);

  PL_register_foreign("archive_open_stream",  3, archive_open_stream, 0);
  PL_register_foreign("archive_property",     3, archive_property,    0);
  PL_register_foreign("archive_close",        1, archive_close,       0);
  PL_register_foreign("archive_next_header",  2, archive_next_header, 0);
  PL_register_foreign("archive_header_prop_", 2, archive_header_prop, 0);
  PL_register_foreign("archive_open_entry",   2, archive_open_entry,  0);
}
