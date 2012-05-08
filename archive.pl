/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(archive,
	  [ archive_open/3,		% +Stream, -Archive, +Options
	    archive_close/1,		% +Archive
	    archive_next_header/2,	% +Archive, -Name
	    archive_open_entry/2	% +Archive, -EntryStream
	  ]).

/** <module> Access several archive formats

This library uses _libarchive_ to access   a variety of archive formats.
The following example lists the entries in an archive:

  ==
  list_archive(File) :-
	open(File, read, Data, [type(binary)]),
	archive_create(Data, Archive, [close_parent(true)]),
	repeat,
	   (   archive_next_header(Archive, Path)
	   ->  format('~w~n', [Path]),
	       fail
	   ;   !,
	       archive_close(Archive)
	   ).
  ==

@see http://code.google.com/p/libarchive/
*/

:- use_foreign_library(foreign(archive4pl)).

%%	archive_open(+Stream, -Archive, +Options) is det.
%
%	If Stream is a (binary) stream   that  contains a valid archive,
%	unify Archive with a handle that  provides access to the content
%	of the archive. Details are   controlled  by Options. Typically,
%	the option close_parent(true) is used  to   close  stream if the
%	archive is closed using archive_close/1.  For other options, the
%	defaults are typically fine. The option format(raw) must be used
%	to process compressed  streams  that   do  not  contain explicit
%	entries (e.g., gzip'ed data) unambibuously.
%
%	  * close_parent(+Boolean)
%	  If this option is =true= (default =false=), Stream is closed
%	  if archive_close/1 is called on Archive.
%
%	  * compression(+Compression)
%	  Support the indicated compression. This option may be used
%	  multiple times to support multiple compression types.
%	  If no compression options are provided, =all= is assumed.
%	  Supported values are =all=, =bzip2=, =compress=, =gzip=,
%	  =lzma=, =none= and =xz=.  The value =all= is default.
%
%	  * format(+Format)
%	  Support the indicated format.  This option may be used
%	  multiple times to support multiple formats. If no format
%	  options are provided, =all= is assumed. Supported values are:
%	  =all=, =ar=, =cpio=, =empty=, =iso9660=, =mtree=, =raw=,
%	  =tar= and =zip=.  The value =all= is default.

%%	archive_close(+Archive) is det.
%
%	Close the archive.  If  close_parent(true)   is  specified,  the
%	underlying stream is closed too.

%%	archive_next_header(+Handle, -Name) is semidet.
%
%	Forward to the next entry of the  archive for which Name unifies
%	with the pathname of the entry. Fails   silently  if the name of
%	the  archive  is  reached  before  success.  Name  is  typically
%	specified if a  single  entry  must   be  accessed  and  unbound
%	otherwise. The following example opens  a   Prolog  stream  to a
%	given archive entry. Note that  _Stream_   must  be closed using
%	close/1 and the archive  must   be  closed using archive_close/1
%	after the data has been used.   See also setup_call_cleanup/3.
%
%	  ==
%	  open_archive_entry(ArchiveFile, Entry, Stream) :-
%	      open(ArchiveFile, read, In, [type(binary)]),
%	      archive_open(In, Archive, [close_parent(true)]),
%	      archive_next_header(Archive, Entry),
%	      archive_open_entry(Archive, Stream).
%	  ==

%%	archive_open_entry(+Archive, -Stream) is det.
%
%	Open the current entry as a stream.  Stream must be closed.

%%	archive_header_data(+Header, +Field, -Value)
%
%	Extract information from the header.

