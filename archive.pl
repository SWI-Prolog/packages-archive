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
	    archive_open_entry/2,	% +Archive, -EntryStream
	    archive_header_property/2,	% +Archive, ?Property
	    archive_extract/3,		% +Archive, +Dir, +Options
	    archive_entries/2		% +Archive, -Entries
	  ]).
:- use_module(library(error)).

/** <module> Access several archive formats

This library uses _libarchive_ to access   a variety of archive formats.
The following example lists the entries in an archive:

  ==
  list_archive(File) :-
	archive_open(File, Archive, []),
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

%%	archive_open(+Data, -Archive, +Options) is det.
%
%	Open the archive in Data and unify  Archive with a handle to the
%	opened archive. Data is either a file  or a stream that contains
%	a valid archive. Details are   controlled by Options. Typically,
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
%
%	Note that the actually supported   compression types and formats
%	may vary depending on the version   and  installation options of
%	the underlying libarchive  library.  This   predicate  raises  a
%	domain  error  if  the  (explicitly)  requested  format  is  not
%	supported.
%
%	@error	domain_error(compression, Compression) if the requested
%		compression type is not supported.
%	@error	domain_error(format, Format) if the requested
%		format type is not supported.

archive_open(stream(Stream), Archive, Options) :- !,
	archive_open_stream(Stream, Archive, Options).
archive_open(Stream, Archive, Options) :-
	is_stream(Stream), !,
	archive_open_stream(Stream, Archive, Options).
archive_open(File, Archive, Options) :-
	open(File, read, Stream, [type(binary)]),
	catch(archive_open_stream(Stream, Archive, [close_parent(true)|Options]),
	      E, (close(Stream), throw(E))).


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

%%	archive_header_property(+Archive, ?Property)
%
%	True when Property is a property of the current header.  Defined
%	properties are:
%
%	  * filetype(-Type)
%	  Type is one of =file=, =link=, =socket=, =character_device=,
%	  =block_device=, =directory= or =fifo=.
%	  * mtime(-Time)
%	  True when entry was last modified at time.
%	  * size(-Bytes)
%	  True when entry is Bytes long.

archive_header_property(Archive, Property) :-
	(   nonvar(Property)
	->  true
	;   header_property(Property)
	),
	archive_header_prop_(Archive, Property).

header_property(filetype(_)).


%%	archive_extract(+ArchiveFile, +Dir, +Options)
%
%	Extract files from the given archive into Dir. Supported
%	options:
%
%	  * remove_prefix(+Prefix)
%	  Strip Prefix from all entries before extracting
%
%	@error	existence_error(directory, Dir) if Dir does not exist
%		or is not a directory.
%	@error  domain_error(path_prefix(Prefix), Path) if a path in
%		the archive does not start with Prefix
%	@tbd	Add options

archive_extract(Archive, Dir, Options) :-
	(   exists_directory(Dir)
	->  true
	;   existence_error(directory, Dir)
	),
	setup_call_cleanup(
	    archive_open(Archive, Handle, Options),
	    extract(Handle, Dir, Options),
	    archive_close(Handle)).

extract(Archive, Dir, Options) :-
	archive_next_header(Archive, Path), !,
	(   archive_header_property(Archive, filetype(file))
	->  (   option(remove_prefix(Remove), Options)
	    ->	(   atom_concat(Remove, ExtractPath, Path)
		->  true
		;   domain_error(path_prefix(Remove), Path)
		)
	    ;	ExtractPath = Path
	    ),
	    directory_file_path(Dir, ExtractPath, Target),
	    file_directory_name(Target, FileDir),
	    make_directory_path(FileDir),
	    setup_call_cleanup(
		archive_open_entry(Archive, In),
		setup_call_cleanup(
		    open(Target, write, Out, [type(binary)]),
		    copy_stream_data(In, Out),
		    close(Out)),
		close(In))
	;   true
	),
	extract(Archive, Dir, Options).
extract(_, _, _).


%%	archive_entries(+Archive, -Paths) is det.
%
%	True when Paths is a list of pathnames appearing in Archive.

archive_entries(Archive, Paths) :-
	setup_call_cleanup(
	    archive_open(Archive, Handle, []),
	    contents(Handle, Paths),
	    archive_close(Handle)).

contents(Handle, [Path|T]) :-
	archive_next_header(Handle, Path), !,
	contents(Handle, T).
contents(_, []).
