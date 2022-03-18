{
Copyright (C) 2017 - 2020  Benito van der Zander (BeniBela)
                           benito@benibela.de
                           www.benibela.de

This file is distributed under under the same license as Lazarus and the LCL itself:

This file is distributed under the Library GNU General Public License
with the following modification:

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent modules,
and to copy and distribute the resulting executable under terms of your choice,
provided that you also meet, for each linked independent module, the terms
and conditions of the license of that module. An independent module is a
module which is not derived from or based on this library. If you modify this
library, you may extend this exception to your version of the library, but
you are not obligated to do so. If you do not wish to do so, delete this
exception statement from your version.

}

(***
  @abstract(Some useful functions or classes)@br

  These are some things that might be added to bbutils, or changed, or discarded.
*)

unit bbutilsbeta;

{$mode objfpc}{$H+}{$ModeSwitch advancedrecords}
{$ifdef FPC_HAS_CPSTRING}
{$define HAS_TYPEHELPERS}
{$ModeSwitch typehelpers}
{$endif}

interface

uses bbutils, sysutils;

//** A generic enumerator that works for any fixed-length collection.
//** @br Drawback: It is always slower than an enumerator custom written for a class. It should use SizeInt, but it cannot use sizeint, when the collection (e.g. TStringList) does not use SizeInt
type
 generic TCommonEnumerator<T> = class
   type TGetCallback = function (i: integer): T of object;
 private
   fpos, flast: integer;
   fget: TGetCallback;
   function GetCurrent: T; inline;
 public
   constructor create(count: integer; callback: TGetCallback);
   function MoveNext: Boolean;
   property Current: T read GetCurrent;
   function GetEnumerator: TCommonEnumerator;
 end;


{$ifdef HAS_TYPEHELPERS}
type
  TBBPcharHelper = type helper for pchar
    function nilMeansInfinity: pchar;
  end;

  TCriticalSectionHelper = {$if FPC_FULLVERSION >= 030200}type{$else}record{$endif} helper for TRTLCriticalSection
    procedure init;
    procedure enter;
    procedure leave;
    procedure done;
  end;

  {$endif}

function objInheritsFrom(o: TObject; c: TClass): boolean; inline;


implementation


function objInheritsFrom(o: TObject; c: TClass): boolean;
begin
  result := assigned(o) and o.InheritsFrom(c);
end;







function TCommonEnumerator.GetCurrent: T;
begin
  result := fget(fpos);
end;

constructor TCommonEnumerator.create(count: integer; callback: TGetCallback);
begin
  fpos := -1;
  flast := count - 1;
  fget := callback;
end;

function TCommonEnumerator.MoveNext: Boolean;
begin
  inc(fpos);
  result := fpos <= flast;
end;

function TCommonEnumerator.GetEnumerator: TCommonEnumerator;
begin
  result := self;
end;









{$ifdef HAS_TYPEHELPERS}

function TBBPcharHelper.nilMeansInfinity: pchar;
begin
  if self = nil then result := pchar(high(PtrUInt))
  else result := self;
end;


procedure TCriticalSectionHelper.init;
begin
  InitCriticalSection(self);
end;

procedure TCriticalSectionHelper.enter;
begin
  EnterCriticalSection(self)
end;

procedure TCriticalSectionHelper.leave;
begin
  LeaveCriticalSection(self);
end;

procedure TCriticalSectionHelper.done;
begin
  DoneCriticalSection(self);
end;

{$endif}

end.
