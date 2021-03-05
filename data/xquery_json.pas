(***
  @abstract(This unit extends the XQuery interpreter with JSONiq)

  Including this unit in the uses clause will add JSONiq support to the xquery.TXQueryEngine.
  I.e.

  1. It will activate the JSONiq syntax extensions for objects {...} and arrays [...]

  2. It will activate the JSON literals: true, false, null

  3. It will declare the jn namespace and add the following functions:@br
     jn:keys, jn:members, jn:is-null, jn:json-doc, jn:null, jn:object, jn:parse-json, jn:size,
     pxp:json, pxp:serialize-json

  @author Benito van der Zander (http://www.benibela.de)
*)

unit xquery_json;

{
Copyright (C) 2008 - 2018 Benito van der Zander (BeniBela)
                          benito@benibela.de
                          www.benibela.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bigdecimalmath, xquery;



implementation

uses jsonscanner, simplehtmltreeparser, bbutils, xquery.namespaces, xquery.internals.common;


function xqFunctionIsNull({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
begin
  result := args[0];
  xqvalueSeqSqueeze(result);
  result := xqvalue(result is TXQValueJSONNull);
end;

function xqFunctionNull({%H-}argc: SizeInt; {%H-}args: PIXQValue): IXQValue;
begin
  result := TXQValueJSONNull.Create();
end;



function xqFunctionObject({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var resobj: TXQValueStringMap;
    procedure merge(another: TXQValue);
    var
      p: TXQProperty;
    begin
      for p in another.getEnumeratorStringPropertiesUnsafe do begin
        if resobj.hasProperty(p.key,nil) then raise EXQEvaluationException.create('jerr:JNDY0003', 'Duplicated key names in '+resobj.jsonSerialize(tnsText)+' and '+another.jsonSerialize(tnsText));
        resobj.setMutable(p.key, p.Value);
      end;
    end;

var v: IXQValue;
  i: SizeInt;
begin
  resobj := TXQValueStringMap.create();
  try
    for i := 0 to argc-1 do
      for v in args[i] do begin
        if v.kind <> pvkObject then raise EXQEvaluationException.create('XPTY0004', 'Expected object, got: '+v.toXQuery());
        merge(v.toValue);
      end;
  except
    on EXQEvaluationException do begin resobj.free; raise; end
  end;
  result := resobj;
end;



function xqFunctionParseJson({%H-}argc: SizeInt; args: PIXQValue): IXQValue; overload;//must be simple due to being in retrieve
var
  parser: TXQJsonParser;
begin
  parser.init;
  parser.options := [jpoAllowMultipleTopLevelItems, jpoJSONiq];
  result := parser.parse(argc, args);
end;



function xqFunctionJSON_Doc(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var
  url: String;
  data: String;
  contenttype: string;
  parser: TXQJsonParser;
begin
  url := args[0].toString;
  if url = '' then exit(xqvalue);

  data := context.staticContext.retrieveFromURI(url, contenttype, 'FODC0002');

  parser.init;
  parser.options := [jpoAllowMultipleTopLevelItems, jpoJSONiq];
  if argc = 2 then parser.setConfigFromMap(args[1]);

  result := parser.parse(data);
end;


function xqFunctionJSON(const context: TXQEvaluationContext; argc: SizeInt; args: PIXQValue): IXQValue;
var
  parser: TXQJsonParser;
  s, contenttype, data: String;
begin
  if assigned(context.staticContext.sender.OnWarningDeprecated) then
    context.staticContext.sender.OnWarningDeprecated(context.staticContext.sender, 'json is deprecated. Use json-doc or parse-json functions.');

  parser.init;
  parser.options := context.staticContext.sender.DefaultJSONParser.options;
  if argc = 2 then parser.setConfigFromMap(args[1]);
  s := args[0].toString;
  if striBeginsWith(s, 'http://') or striBeginsWith(s, 'https://') or striBeginsWith(s, 'file://') then begin
    data := context.staticContext.retrieveFromURI(s, contenttype, 'FODC0002');
  end else data := s;
  result := parser.parse(data);
end;

function xqFunctionKeys({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: IXQValue;
  keyset: TXQHashsetStr;
begin
  keyset.init;
  for v in args[0] do
    if v.kind = pvkObject then v.enumeratePropertyKeys(keyset);
  result := xqvalue(keyset);
  keyset.done;
end;


function xqFunctionMembers({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  v: IXQValue;
  ara: TXQValueJSONArray;
  i: SizeInt;
  list: TXQVList;
begin
  list := TXQVList.create();
  for v in args[0] do
    if v is TXQValueJSONArray then begin
      ara := v as TXQValueJSONArray;
      for i := 0 to ara.seq.Count-1 do
        list.add(ara.seq[i]);
    end;
  xqvalueSeqSqueezed(result, list)
end;

function xqFunctionSize({%H-}argc: SizeInt; args: PIXQValue): IXQValue;
var
  a: IXQValue;
begin
  a := args[0];
  if (a.kind = pvkSequence) and (a.getSequenceCount = 1) then a := a.get(1);
  if a.getSequenceCount = 0 then exit(xqvalue());
  if not (a is TXQValueJSONArray) then raise EXQEvaluationException.create('pxp:ARRAY', 'Expected array, got: '+a.toXQuery());
  result := xqvalue((a as TXQValueJSONArray).seq.Count);
end;


var jn, pxp, libjn: TXQNativeModule;
    XMLNamespace_JSONiqFunctions, XMLNamespace_JSONiqLibraryFunctions: TNamespace;

initialization
  AllowJSONDefaultInternal := true;
  XMLNamespace_JSONiqFunctions:=TNamespace.makeWithRC1('http://jsoniq.org/functions', 'jn');
  GlobalStaticNamespaces.add(XMLNamespace_JSONiqFunctions);
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/types', 'js');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/function-library', 'libjn');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/errors', 'jerr');
  //XMLNamespace_JSONiqTypes:=TNamespace.create('http://jsoniq.org/updates', 'jupd');


  with globalTypes do begin
    jn := TXQNativeModule.Create(XMLNamespace_JSONiqFunctions);
    TXQueryEngine.registerNativeModule(jn);
    jn.registerBasicFunction('keys', @xqFunctionKeys, [itemStar, stringStar]);
    jn.registerBasicFunction('members', @xqFunctionMembers, [itemStar, itemStar]);

    //TODO: fn:string/fn:data errors
    //TODO:   6.6. jn:decode-from-roundtrip 6.7. jn:encode-for-roundtrip
    //TODO:  6.9. jn:json-doc
  //  jn.registerFunction('encode-for-roundtrip', @xqFunctionEncode_For_Roundtrip, ['jn:encode-for-roundtrip($items as item()*) as json-item()* ', 'jn:encode-for-roundtrip($items as item()*, $options as object()) as json-item()* ']);
    jn.registerBasicFunction('is-null', @xqFunctionIsNull, [item, boolean]);
    jn.registerFunction('json-doc', @xqFunctionJSON_Doc, [stringOrEmpty, jsonItemOrEmpty], [xqcdContextOther]);
    jn.registerBasicFunction('null', @xqFunctionNull, [null]);
    jn.registerFunction('object', 0, -1, @xqFunctionObject, []); //deprecated
    jn.registerFunction('parse-json', @xqFunctionParseJson).setVersionsShared([stringOrEmpty, jsonitemStar],  [stringOrEmpty, map, jsonItemStar]);
    jn.registerBasicFunction('size', @xqFunctionSize, [arrayOrEmpty, integerOrEmpty]);

    pxp := TXQueryEngine.findNativeModule(XMLNamespaceURL_MyExtensionsMerged);
    pxp.registerFunction('json', @xqFunctionJson, [xqcdContextOther]).setVersionsShared([stringt, itemStar],  [stringt, map, itemStar]);

    {[itemStar, map]
descendant-arrays
[itemStar, arrayStar]
descendant-objects
[itemStar, mapStar]
descendant-pairs
[itemStar, itemStar]
flatten
[itemStar, itemStar]
intersect
[itemStar, map]
project
[itemStar, stringStar, itemStar]
remove-keys
[itemStar, stringStar, itemStar]
values
[itemStar, itemStar]}

    XMLNamespace_JSONiqLibraryFunctions:=TNamespace.makeWithRC1('http://jsoniq.org/function-library', 'libjn');
    libjn := TXQNativeModule.create(XMLNamespace_JSONiqLibraryFunctions);
  //new function from 1.0.1 not working libjn.registerInterpretedFunction('accumulate', '($seq as item()*) as object()', '{| for $key in jn:keys($seq) return { $key : $seq($key) }  |}');
    {my own with 1.0.1 semantics} libjn.registerInterpretedFunction('accumulate', '($seq as item()*) as object()',
      'jn:object( ' +
        ' let $o := for $p in $seq '+
                    'return if ($p instance of object()) then $p else ()'+
           ', $all-keys := for $object in $o return jn:keys($object)' +
         'for $distinct-key in distinct-values($all-keys) '+
         'let $values := $o($distinct-key) '+
         'return if (count($values) eq 1) then { $distinct-key : $values } else { $distinct-key : [ $values ] } )');
    //old accumulate function: libjn.registerInterpretedFunction('accumulate', '($o as object()*) as object()', 'jn:object( let $all-keys := for $object in $o return jn:keys($object) for $distinct-key in distinct-values($all-keys) let $values := $o($distinct-key) return if (count($values) eq 1) then { $distinct-key : $values } else { $distinct-key : [ $values ] } )');

    libjn.registerInterpretedFunction('descendant-arrays', '($seq as item()*) as array()*',
        'for $i in $seq ' +
        'return typeswitch ($i) ' +
        'case array() return ( ' +
        '  $i, ' +
        '  libjn:descendant-arrays(jn:members($i)) ' +
        ') ' +
        'case object() ' +
        '    return libjn:descendant-arrays(libjn:values($i)) ' +
        'default return () ');

    libjn.registerInterpretedFunction('descendant-objects', '($seq as item()*) as object()*',
        'for $i in $seq ' +
        'return typeswitch ($i) ' +
        'case object() return ( ' +
        '  $i, ' +
        '  libjn:descendant-objects(libjn:values($i)) ' +
        ') ' +
        'case array() return ' +
        '    libjn:descendant-objects(jn:members($i)) ' +
        'default return () ');
    libjn.registerInterpretedFunction('descendant-pairs', '($seq as item()*) as item()*',
        'for $i in $seq ' +
        'return typeswitch ($i) ' +
        'case object() return ' +
        '  for $k in jn:keys($i) ' +
        '  let $v := $i($k) ' +
        '  return ( ' +
        '    { $k : $v }, ' +
        '    libjn:descendant-pairs($v) ' +
        '  ) ' +
        'case array() return ' +
        '  libjn:descendant-pairs(jn:members($i)) ' +
        'default return () '
    );
    libjn.registerInterpretedFunction('flatten', '($seq as item()*) as item()* ',
      'for $i in $seq ' +
      'return ' +
      '  typeswitch ($i) ' +
      '  case array() return libjn:flatten(jn:members($i)) ' +
      '  default return $i '
    );
    libjn.registerInterpretedFunction('intersect', '($seq as item()*) as object()',
      '{| ' +
      '  let $objects := $seq[. instance of object()] ' +
      '  for $key in jn:keys(($objects)[1]) ' +
      '  where every $object in ($objects)[position() > 1] ' +
      '        satisfies exists(index-of(jn:keys($object), $key)) ' +
      '  return { $key : $objects($key) } ' +
      '|} '
    );


    libjn.registerInterpretedFunction('project', '($seq as item()*, $keys as xs:string*) as item()*',
      'for $item in $seq ' +
      'return typeswitch ($item) ' +
      '       case $object as object() return ' +
      '       {| ' +
      '         for $key in jn:keys($object) ' +
      '         where some $to-project in $keys satisfies $to-project eq $key ' +
  //    '         let $value := $object($key) ' + requires XQuery 3
      '         return { $key : $object($key) } ' +
      '       |} ' +
      '       default return $item ');

    libjn.registerInterpretedFunction('remove-keys', '($seq as item()*, $keys as xs:string*) as item()*',
      'for $item in $seq ' +
      'return typeswitch ($item) ' +
      '       case $object as object() return ' +
      '       {| ' +
      '         for $key in jn:keys($object) ' +
      '         where every $to-remove in $keys satisfies $to-remove ne $key ' +
   //   '         let $value := $object($key) ' +
      '         return { $key : $object($key) } ' +
      '       |} ' +
      '       default return $item ');

    libjn.registerInterpretedFunction('values', '($seq as item()*) as item()*',
      'for $i in $seq '+
      'for $k in jn:keys($i) '+
      'return $i($k)');


    TXQueryEngine.registerNativeModule(libjn);
  end;
finalization
  libjn.free;
  jn.free;
  XMLNamespace_JSONiqFunctions._Release;
  XMLNamespace_JSONiqLibraryFunctions._Release;
end.

