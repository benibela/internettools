{Copyright (C) 2013  Benito van der Zander

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}

{** @abstract(This unit contains a mock internet access class, i.e. a class that simulates an internet connection for e.g. unit tests)

    See TMockInternetAccess}

unit mockinternetaccess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, internetaccess;



type TMockTransfer = procedure (const method: string; const url: TDecodedUrl; const postdata: TInternetAccessDataBlock; var outdata: string) of object;
{ TInternetAccessNonSense }

{ TMockInternetAccess }
(***
    @abstract(This class simulates an internet access, e.g. for unit tests)

There are three ways to use it:
@unorderedList(
@item(Just use it, without any changes. Then it will simulate a server returning DefaultMockPage on every request)
@item(Assign a method to the event OnTransfer, which will be called by every request, and simulate whatever you want )
@item(Assign a path to SimulatedServerPath, then it will simulate a server, returning the files from that directory)
)
*)
TMockInternetAccess = class(TInternetAccess)
  constructor create; override;
  procedure doTransferUnChecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock); override;

  destructor Destroy; override;
public
  OnTransfer: TMockTransfer;
  SimulatedServerPath: string;
end;

var DefaultMockPage: string = '<html>Internet disabled</html>';
implementation
uses bbutils;

{ TInternetAccessNonSense }

constructor TMockInternetAccess.create;
begin
  init
end;

procedure TMockInternetAccess.doTransferUnChecked(method: string; const url: TDecodedUrl; const data: TInternetAccessDataBlock);
var result: string;
begin
  if SimulatedServerPath = '' then result := DefaultMockPage
  else result := strLoadFromFile(SimulatedServerPath + url.path + url.params);
  if Assigned(OnTransfer) then OnTransfer(method, url, data, result);
  if result <> '' then writeBlock(pointer(result)^, length(result));
end;

destructor TMockInternetAccess.Destroy;
begin
  inherited Destroy;
end;


end.

