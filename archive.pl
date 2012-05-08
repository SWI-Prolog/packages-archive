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

*/

:- use_foreign_library(foreign(archive4pl)).

%%	archive_open(+Data, -Handle, +Options) is det.
%
%	Create an access handle to an archive. The following options are
%	defined:
%
%	  * compression(+Compression)
%	  Support the indicated compression. This option may be used
%	  multiple times to support multiple compression types.
%	  If no compression options are provided, =all= is assumed.
%	  Supported values are:
%
%	    - =all=
%	    - =bzip2=
%	    - =compress=
%	    - =gzip=
%	    - =lzma=
%	    - =none=
%	    - =xz=
%
%	  * format(+Format)
%	  Support the indicated format.  This option may be used
%	  multiple times to support multiple formats. If no format
%	  options are provided, =all= is assumed. Supported values are:
%
%	    - =all=
%	    - =ar=
%	    - =cpio=
%	    - =empty=
%	    - =iso9660=
%	    - =mtree=
%	    - =raw=
%	    - =tar=
%	    - =zip=
%
%	  * options ...?
%
%	@param ReadWrite is one of =read= or =write=

%%	archive_next_header(+Handle, -Name)
%
%	NextHeaderHandle is a handle to the next header in the archive.

%%	archive_header_data(+Header, +Field, -Value)
%
%	Extract information from the header.

%%	archive_close(+Archive)
%
%	Close the archive.  If  close_parent(true)   is  specified,  the
%	underlying stream is closed too.

%%	archive_open_entry(+Archive, -Stream)
%
%	Open the current entry as a stream.  Stream must be closed.
