type THashInfo = record
  //same hash as python, see:
  //https://stackoverflow.com/questions/56227419/why-does-pythons-hash-of-infinity-have-the-digits-of-π
  //https://docs.python.org/3/library/stdtypes.html#hashing-of-numeric-types
  //https://github.com/python/cpython/blob/master/Python/pyhash.c
  const hash_pos_inf = 314159;
  const hash_neg_inf = cardinal.MaxValue - hash_pos_inf;
  const hash_nan = high(integer);
  const modulus_bits = {$ifdef xxxCPU64xxxxx}61 no, only use 32 bit version{$else}31{$endif};
  const modulus = (1 shl modulus_bits) - 1;
  const bigdecimalInverse = 968029830;
end;


class function TXQValueUndefined.classKind: TXQValueKind;
begin
  result := pvkUndefined;
end;

function TXQValueUndefined.isUndefined: boolean;
begin
  Result:=true;
end;

function TXQValueUndefined.toArray: TXQVArray;
begin
  result := nil;
end;

function TXQValueUndefined.toXQVList: TXQVList;
begin
  Result:=TXQVList.Create;
end;

function TXQValueUndefined.getSequenceCount: SizeInt;
begin
  Result := 0 ;
end;

function TXQValueUndefined.clone: IXQValue;
begin
  result := xqvalue();
end;

function TXQValueUndefined.GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  TXQValueEnumeratorPtrUnsafe.clear(result);
end;

procedure TXQValueUndefined.jsonSerialize(var serializer: TXQSerializer);
begin
  serializer.append('null');
end;

procedure TXQValueUndefined.xmlSerialize(var serializer: TXQSerializer);
begin
  serializer.appendXMLEmptyElement(serializer.sequenceTag);
end;

procedure TXQValueUndefined.adaptiveSerialize(var serializer: TXQSerializer);
begin
  serializer.append('()');
end;

function TXQValueUndefined.map(const q: string): IXQValue;
begin
  ignore(q);
  Result:=xqvalue();
end;

function TXQValueUndefined.map(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  ignore(q); ignore(vs);
  Result:=xqvalue();
end;

function TXQValueUndefined.map(const q: string; const vs: array of string): IXQValue;
begin
  ignore(q); ignore(vs);
  Result:=xqvalue();
end;

function TXQValueUndefined.filter(const q: string): IXQValue;
begin
  ignore(q);
  Result:=xqvalue();
end;

function TXQValueUndefined.filter(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  ignore(q); ignore(vs);
  Result:=xqvalue();
end;

function TXQValueUndefined.filter(const q: string; const vs: array of string): IXQValue;
begin
  ignore(q); ignore(vs);
  Result:=xqvalue();
end;


{ TXQValueUndefined }

function TXQValue.toBoolean: boolean;
begin
 result := false;
end;

function TXQValue.toBooleanEffective: boolean;
begin
  result :=  toBoolean;
end;

function TXQValue.toInt64: int64;
  function myStrToInt(const s:string):int64;
  var tempf:xqfloat;
  begin
    if not TryStrToInt64(s, result) then
      if TryStrToFloat(s, tempf, XQFormats) then result:=trunc(tempf)
      else result:=0;
  end;
var
  temp: String;
begin
  temp := trim(toString);
  if temp = '' then exit(0);
  result := myStrToInt(temp);
end;

function TXQValue.toFloat: xqfloat;
begin
  result:= xqfloat.parse(toString);
end;

function TXQValue.toString: string;
begin
  result := '';
end;

function TXQValue.toJoinedString(const sep: string): string;
begin
  ignore(sep);
  result := toString;
end;

function TXQValue.toDateTime: TDateTime;
begin
  result := 0;
end;

function TXQValue.toNode: TTreeNode;
begin
  result := nil;
end;

function TXQValue.toArray: TXQVArray;
begin
  result := nil;
  setlength(result, 1);
  result[0] := self;
end;

function TXQValue.toBinaryBytes: TBytes;
begin
  raiseXPTY0004TypeError(self, 'binary');
end;

{$ImplicitExceptions off}
constructor TXQValue.create(atypeAnnotation: TXSType);
begin
  ftypeAnnotation := atypeAnnotation;
end;
{$ImplicitExceptions on}

constructor TXQValue.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  ignore(atypeAnnotation); ignore(value);
  raise EXQEvaluationException.create('XQST0052', 'Cannot create an instance of the abstract base type anyType (TXQValue)');
end;

type TNewinstanceFunc = function: tobject of object;
class function TXQValue.newinstance: tobject;
var k: TXQValueKind;
  hackMethod: TMethod;
  head: PPointer;
begin
  k := classKind;
  with threadLocalCache do begin
    if commonValues[k].head <> nil then begin
      head := commonValues[k].head;
      result := TXQValue(head);
      commonValues[k].head := head^;
      head^ := commonValues[k].vmt;
      //A normal object needs to be cleaned up and initialized. We do the clean up in the destructor and can skip it here
      //hackMethod := tmethod(@CleanupInstance);
      //hackMethod.Data := result;
      //TProcedureOfObject(hackMethod)();
      //InitInstance(result);
    end else begin
      hackMethod.Data := self;
      hackMethod.Code := @TObject.newinstance;
      result := TNewinstanceFunc(hackMethod)();
    end;
  end;
end;


procedure TXQValue.FreeInstance;
var
  k: TXQValueKind;
  pself: ppointer;
begin
  k := classKind;
  with threadLocalCache do begin
    if runningEngines > 0 then begin
      pself := ppointer(self);
      if commonValues[k].vmt = nil then commonValues[k].vmt := pself^
      else assert(commonValues[k].vmt = pself^);
      pself^ := commonValues[k].head;
      commonValues[k].head := pself;
    end else Freemem(pointer(self));
  end;
end;

function TXQValue.kind: TXQValueKind;
begin
  result := classKind;
end;

function TXQValue.typeName: string;
begin
  result := ftypeAnnotation.name;
end;


class function TXQValue.classKind: TXQValueKind;
begin
  result := pvkUndefined;
end;


function TXQValue.instanceOf(const typ: TXSType): boolean;
begin
  result := typeAnnotation.derivedFrom(typ);
end;

procedure TXQValue.raiseInternalErrorObjectExpected(const functionname: string);
begin
  raiseInternalError('Function ' + functionname+ ' can only be called on an object, but was called on ' + toXQuery);
end;

procedure TXQValue.enumeratePropertyKeys(var keyset: TXQHashsetStr);
begin
  raiseInternalErrorObjectExpected('enumeratePropertyKeys');
end;

function TXQValue.enumeratePropertyKeys: IXQValue;
var keyset: TXQHashsetStr;
begin
  keyset.init;
  enumeratePropertyKeys(keyset);
  result := xqvalue(keyset);
  keyset.done;
end;

function TXQValue.enumeratePropertyValues: IXQValue;
begin
  raiseInternalErrorObjectExpected('enumeratePropertyValues');
  result := nil
end;


procedure TXQValue.prepareInternetRequest(out method, url, post: string; internet: TInternetAccess);
var h: IXQValue;
  value: String;
  name: RawByteString;
begin
  url := getProperty('url').toString;
  method := getProperty('method').toString;
  post := getProperty('post').toString;
  for h in getProperty('headers') do begin
    value := h.toString;
    name := strSplitGet(':', value);
    internet.additionalHeaders.Values[trim(name)] := trim(value);
  end;
end;

function TXQValue.setImmutable(const name, value: IXQValue): TXQValueMapLike;
begin
  result := setImmutable(name.toString, value);
end;

function TXQValue.setImmutable(const name: string; const v: IXQValue): TXQValueMapLike;
begin
  raiseInternalErrorObjectExpected('setImmutable');
  result := nil;
end;

function TXQValue.setImmutable(const name: string; const s: string): TXQValueMapLike;
begin
  result := setImmutable(xqvalue(name), xqvalue(s));
end;

function TXQValue.setImmutable(const props: PString; len: SizeInt; const v: IXQValue): TXQValue;
begin
  raiseInternalErrorObjectExpected('setImmutable (or array)');
  result := nil;
end;

function TXQValue.hashCode: uint32;
begin
  result := 0
end;

function TXQValue.toDecimal: BigDecimal;
begin
  if not tryStrToBigDecimal(toString, @result) then
    setZero(result);
end;

function TXQValue.isUndefined: boolean;
begin
  result := false;
end;

function TXQValue.toXQVList: TXQVList;
begin
  result:=TXQVList.Create;
  result.add(self);
end;

function TXQValue.toXQuery: string;
  function escapeToXQueryString(const s: string): string;
  begin
    result := StringReplace(s, '&', '&amp;', [rfReplaceAll]);
    if pos('"', result) = 0 then exit('"' + result + '"');
    if pos('''', result) = 0 then exit('''' + result + '''');
    result := StringReplace(s, '"', '""', [rfReplaceAll]);
    result := '"' + result + '"';
  end;

  function qualifiedTypeName(const arg: string; const ignoreDefault: string = ''): string;
  begin
    if typeAnnotation.schema.url <> XMLNamespaceURL_XMLSchema then
       exit('Q{'+typeAnnotation.schema.url + '}' + typeName+'('+arg+')');
    if typeName = ignoreDefault then exit(arg);
    exit('xs:'+typeName+'('+escapeToXQueryString(toString)+')');
  end;

var
  pv: PIXQValue;
  first: Boolean;
  enum: TXQValueEnumeratorPtrUnsafe;
  n: TTreeNode;
begin
  case kind of
    pvkUndefined:  result := '()';
    pvkBoolean:    result := qualifiedTypeName(IfThen(toBoolean, 'true()', 'false()'), 'boolean');
    pvkNull:       result := 'null';
    pvkInt64:      result := qualifiedTypeName(toString, 'integer');
    pvkFloat:      result := qualifiedTypeName(toString, '');
    pvkBigDecimal: result := qualifiedTypeName(toString, IfThen(IsIntegral(toDecimal), 'integer', 'decimal') );
    pvkString, pvkBinary: result := qualifiedTypeName(escapeToXQueryString(toString), 'string' );
    pvkQName, pvkDateTime:
      result := qualifiedTypeName(escapeToXQueryString(toString), '' );

    pvkNode:       begin
      n := toNode;
      case n.typ of
        tetDocument: result := 'document { ' + n.outerXML() + ' }';
        tetAttribute: result := 'attribute ' + n.getNodeName() + ' {' + escapeToXQueryString(TTreeAttribute(n).realvalue) +  '}';
        else result := toNode.outerXML();
      end;
    end;
    pvkObject:     result := jsonSerialize(tnsXML);
    pvkArray, pvkSequence:      begin
      if kind = pvkSequence then begin
        result := '(';
        enum := GetEnumeratorPtrUnsafe;
      end else begin
        result := '[';
        enum := (self as TXQValueJSONArray).GetEnumeratorMembersPtrUnsafe;
      end;
      first := true;
      for pv in enum do begin
        if not first then result += ', ';
        result += pv^.toXQuery;
        first := false;
      end;
      if kind = pvkSequence then result += ')' else result += ']';
    end;
    else result := 'serialization failed??';
  end;
end;

function TXQValue.toValue: TXQValue;
begin
  result := self;
end;

function TXQValue.getSequenceCount: SizeInt;
begin
  result := 1;
end;

function TXQValue.get(i: SizeInt): IXQValue;
begin
  if i = 1 then exit(self);
  exit(xqvalue);
end;

function TXQValue.hasProperty(const name: string; value: PXQValue): boolean;
begin
  result := false;
end;

function TXQValue.hasProperty(const name: ixqvalue; value: PXQValue): boolean;
begin
  result := false;
end;

function TXQValue.getProperty(const name: IXQValue): IXQValue;
begin
  ignore(name);
  result := xqvalue();
end;

function TXQValue.getProperty(const name: string): IXQValue;
begin
  ignore(name);
  result := xqvalue();
end;

function TXQValue.getPropertyKeyKind: TXQMapPropertyKeyKind;
begin
  result := xqmpkkStandardKeys;
  raise EXQEvaluationException.create('pxp:JSON', 'Attempted to key kind of non-object');
end;

function TXQValue.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  result := nil;
  raise EXQEvaluationException.create('pxp:JSON', 'Attempted to read property of non-object');
end;

function TXQValue.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  result := nil;
  raise EXQEvaluationException.create('pxp:JSON', 'Attempted to read property of non-object');
end;

function TXQValue.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  result := nil;
  raise EXQEvaluationException.create('pxp:JSON', 'Attempted to read property of non-object');
end;

function TXQValue.getInternalDateTimeData: PXQValueDateTimeData;
begin
  raiseXPTY0004TypeError(self, 'internal datetime');
  result := nil;
end;

function TXQValue.Size: SizeInt;
begin
  raise EXQEvaluationException.create('pxp:INTERNAL', 'Only arrays and objects have a size');
  result := 0;
end;

function TXQValue.GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  TXQValueEnumeratorPtrUnsafe.makesingleelement(self, result);
end;

function TXQValue.GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  raise EXQEvaluationException.create('pxp:INTERNAL', 'Array enumerator called on non array.');
  result := default(TXQValueEnumeratorPtrUnsafe);
end;

function TXQValue.GetEnumeratorArrayTransparentUnsafe: TXQValueEnumeratorArrayTransparentUnsafe;
begin
  result.currentEnumerator := 0;
  setlength(result.ptrs, 1);
  result.ptrs[0] := GetEnumeratorPtrUnsafe;
end;

function TXQValue.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  if scontext.strictTypeChecking then raiseXPTY0004TypeError(self, 'float/double');
  result := self.toFloat;
end;

function TXQValue.debugAsStringWithTypeAnnotation(textOnly: boolean = true): string;
var
  i: SizeInt;
  prop: TXQStandardProperty;
  first: Boolean;
begin
  case self.kind of
    pvkSequence: begin
      result := 'sequence: (';
      if TXQValueSequence(self).seq.Count > 0 then begin
        result += TXQValueSequence(self).seq[0].debugAsStringWithTypeAnnotation(textOnly){%H-};
        for i:=1 to TXQValueSequence(self).seq.Count-1 do
          result += ', ' + (TXQValueSequence(self).seq[i]).debugAsStringWithTypeAnnotation(textOnly){%H-};
      end;
      result+=')';
    end;
    pvkArray      : begin
      result := 'array: [';
      if TXQValueSequence(self).seq.Count > 0 then begin
        result += TXQValueSequence(self).seq[0].debugAsStringWithTypeAnnotation(textOnly){%H-};
        for i:=1 to TXQValueSequence(self).seq.Count-1 do
          result += ', ' + (TXQValueSequence(self).seq[i]).debugAsStringWithTypeAnnotation(textOnly){%H-};
      end;
      result+=']';
    end;
    pvkObject: begin
      result := 'object: {';
      first := true;
      for prop in getEnumeratorPropertiesUnsafe do
        if first then begin
          result += prop.key.debugAsStringWithTypeAnnotation+': '+prop.Value.debugAsStringWithTypeAnnotation(textOnly){%H-};
          first := false
        end else result += ', '+prop.key.debugAsStringWithTypeAnnotation+': '+prop.Value.debugAsStringWithTypeAnnotation(textOnly){%H-};
      result += '}';
    end;
    pvkNode: if textOnly then result := typeName+': '+toString else result := typeName + ': '+toNode.outerXML();
    else result := typeName+': '+toString;
  end;
end;

function TXQValue.jsonSerialize(nodeFormat: TTreeNodeSerialization; insertWhitespace: boolean): string;
var serializer: TXQSerializer;
begin
  serializer.init(@result);
  serializer.nodeFormat := nodeFormat;
  if insertWhitespace then serializer.insertWhitespace := xqsiwIndent
  else serializer.insertWhitespace := xqsiwConservative;
  jsonSerialize(serializer);
  serializer.final;
end;

function TXQValue.xmlSerialize(nodeFormat: TTreeNodeSerialization; sequenceTag: string = 'seq'; elementTag: string = 'e'; objectTag: string = 'object'): string;
var serializer: TXQSerializer;
begin
  serializer.init(@result);
  serializer.nodeFormat := nodeFormat;
  serializer.sequenceTag := sequenceTag;
  serializer.elementTag := elementTag;
  serializer.objectTag := objectTag;
  xmlSerialize(serializer);
  serializer.final;
end;




procedure TXQValue.jsonSerialize(var serializer: TXQSerializer);
  procedure objectJsonSerialize(var serializer: TXQSerializer);
  var
    prop: TXQProperty;
    isStringMap: Boolean;
    nameCheck: ^TXQHashmapStrStr;
    enumerator: TXQValueStringPropertyEnumerator;
  begin
    isStringMap := getPropertyKeyKind = xqmpkkStringKeys;
    if isStringMap or serializer.allowDuplicateNames then nameCheck := nil
    else new(nameCheck,init);
    enumerator := getEnumeratorStringPropertiesUnsafe;
    try
      if not enumerator.MoveNext then serializer.appendJSONEmptyObject
      else with serializer do begin
        appendJSONObjectStart;
        while true do begin
          prop := enumerator.Current;
          if assigned(nameCheck) then begin
            if nameCheck.contains(prop.key) then
              raiseXQEvaluationError('SERE0022', 'Duplicate key names when serializing.', self);
            nameCheck.include(prop.key, '');
          end;
          appendJSONObjectKeyColon(prop.key);
          prop.Value.jsonSerialize(serializer);
          if not enumerator.MoveNext then break;
          appendJSONObjectComma
        end;
        appendJSONObjectEnd;
      end;
    finally
      if assigned(nameCheck) then nameCheck^.done;
      enumerator.free;
    end;
  end;

begin
  case kind of
    pvkObject: objectJsonSerialize(serializer);
    else serializer.appendJSONString(toString);
  end;
end;

procedure TXQValue.xmlSerialize(var serializer: TXQSerializer);
  procedure objectXmlSerialize(var serializer: TXQSerializer);
  var
    first: Boolean;
    prop: TXQProperty;
    tempName: String;
  begin
    first := true;
    with serializer do begin
      for prop in getEnumeratorStringPropertiesUnsafe do begin
        if first then begin
          appendXMLElementStartTag(objectTag);
          first := false;
        end;
        tempName := prop.key;
        if isValidNCName(tempName) then
          appendXMLElementStartTag(tempName)
        else begin
          appendXMLElementStartOpen('_');
          appendXMLElementAttribute('key', tempName);
          appendXMLElementStartClose();
          tempName := '_';
        end;
        prop.Value.xmlSerialize(serializer);
        appendXMLElementEndTag(tempName);
      end;
      if first then appendXMLEmptyElement(objectTag) //no properties
      else appendXMLElementEndTag(objectTag);
    end;
  end;
begin
  case kind of
    pvkObject: objectXmlSerialize(serializer);
    else serializer.appendXMLText(toString);
  end;
end;

procedure TXQValue.adaptiveSerialize(var serializer: TXQSerializer);
  procedure objectAdaptiveSerialize(var serializer: TXQSerializer);
  var
    prop: TXQStandardProperty;
    first: Boolean;
  begin
    first := true;
    with serializer do begin
      append('map');
      for prop in getEnumeratorPropertiesUnsafe do begin
        if not first then appendJSONObjectComma
        else begin
          first := false;
          appendJSONObjectStart;
        end;
        prop.key.adaptiveSerialize(serializer);
        append(':');
        prop.Value.adaptiveSerialize(serializer);
      end;
      if not first then appendJSONObjectEnd
      else appendJSONEmptyObject;
    end;
  end;
var
  withType: Boolean;
begin
  case kind of
    pvkObject: objectAdaptiveSerialize(serializer);
    else
      withType := not (typeAnnotation.derivedFrom([baseSchema.string_, baseSchema.untypedAtomic, baseSchema.anyURI]));
      if withType then serializer.appendTypeNameFunctionStart(typeAnnotation);
      serializer.appendXQueryString(toString);
      if withType then serializer.append(')');
  end;
end;

function TXQValue.serialize(const context: TXQEvaluationContext): string;
begin
  result := globalSerializationCallback(context, self);
end;


function TXQValue.stringifyNodes: IXQValue;
  function objectStringifiedNodes: IXQValue;
  var
    prop: TXQProperty;
    newMap: TXQValueStandardMap;
    newStringMap: TXQValueStringMap;
    prop2: TXQStandardProperty;
  begin
    case getPropertyKeyKind of
      xqmpkkStringKeys: begin
        newStringMap := TXQValueStringMap.create();
        for prop in getEnumeratorStringPropertiesUnsafe do
          newStringMap.setMutable(prop.key, prop.Value.stringifyNodes);
        result := newStringMap;
      end;
      xqmpkkStandardKeys: begin
        newMap := TXQValueStandardMap.create();
        for prop2 in getEnumeratorPropertiesUnsafe do
          newMap.setMutable(prop2.key, prop2.Value.stringifyNodes);
        result := newMap;
      end;
    end;
  end;

begin
  case kind of
    pvkObject: result := objectStringifiedNodes;
    else result := self;
  end;
end;

function TXQValue.hasNodes: boolean;
  function objectHasNodes: boolean;
  var
    v: TXQValue;
  begin
    result := false;
    for v in getEnumeratorPropertyValuesUnsafe do
      if v.hasNodes then
        exit(true);
  end;

begin
  case kind of
    pvkObject: result := objectHasNodes;
    else result := false;
  end;
end;

function TXQValue.clone: IXQValue;
begin
  raise EXQEvaluationException.Create('pxp:INTERNAL', 'Clone on "abstract" base class called.');
  result := nil;
end;

function TXQValue.query(const q: string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.add('_', self);
  result := xquery.query(q);
  defaultQueryEngine.VariableChangelog.removeLast;
end;

function TXQValue.query(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.add('_', self);
  result := xquery.query(q, vs);
  defaultQueryEngine.VariableChangelog.removeLast;
end;

function TXQValue.query(const q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.add('_', self);
  result := xquery.query(q, vs);
  defaultQueryEngine.VariableChangelog.removeLast;
end;

function TXQValue.map(const q: string): IXQValue;
begin
  result := defaultQueryEngine.evaluateXQuery(q, self);
end;

function TXQValue.map(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := map(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function TXQValue.map(const q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := map(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function TXQValue.filter(const q: string): IXQValue;
var
  term: TXQuery;
  tempContext: TXQEvaluationContext;
  list: TXQVList;
  seq: TXQValueSequence;
  tempFilter: TXQPathMatchingStepFilter;
begin
  term := defaultQueryEngine.parseTerm(q, xqpmXQuery3_1, defaultQueryEngine.StaticContext);
  try
    tempContext := defaultQueryEngine.getEvaluationContext();

    tempFilter.filter := term.getTerm;
    tempFilter.dependencies := tempFilter.filter.getContextDependencies;
    list := TXQVList.create();
    seq := TXQValueSequence.create(list);
    result := seq;
    TXQueryEngine.filterSequence(self, list, tempFilter, tempContext);
    xqvalueSeqSqueeze(result);

  finally
    term.free;
  end;
end;

function TXQValue.filter(const q: string; const vs: array of ixqvalue): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := filter(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function TXQValue.filter(const q: string; const vs: array of string): IXQValue;
begin
  defaultQueryEngine.VariableChangelog.pushOpenArray(vs);
  result := filter(q);
  defaultQueryEngine.VariableChangelog.popAll();
end;

function TXQValue.order(const q: string): IXQValue;
begin
  ignore(q);
  result := self; //singleton is always ordered
end;

function TXQValue.retrieve(): IXQValue;
var tempContext: TXQEvaluationContext;
  resseq: TXQValueSequence;
  function dataToXQV(const data, uri, contenttype: string): ixqvalue;
  var
    f: TInternetToolsFormat;
  begin
    f := guessFormat(data, uri, contenttype);
    case f of
      itfJSON:
        exit(defaultQueryEngine.DefaultJSONParser.parse(data));
      itfUnknown, itfPlainText:
        exit(TXQValueString.create(baseSchema.untypedAtomic, data));
      itfXML, itfHTML, itfXMLPreparsedEntity:
        result := xqvalue(tempContext.parseDoc(data, uri, contenttype));
    end;
  end;

  procedure handle(const v: IXQValue);
  var
    contenttype: string;
    uri: String;
    method: string;
    post: string;
    tempHeaders: String;
    data: string;
  begin
    case v.kind of
      pvkObject: begin
         tempHeaders := defaultInternet.additionalHeaders.Text;
         v.toValue.prepareInternetRequest(method, uri, post, defaultInternet);
         data := internetaccess.httpRequest(method, uri, post);
         defaultInternet.additionalHeaders.Text := tempHeaders;
         resseq.add(dataToXQV (data, uri, defaultInternet.getLastContentType));
      end;
      else begin
        uri := v.toString;
        if not strIsAbsoluteURI(uri) then raise EXQEvaluationException.create('pxp:RETRIEVE', 'Need absolute URI to retrieve, got: '+uri);
        data := defaultQueryEngine.StaticContext.retrieveFromURI(uri, contenttype, 'pxp:RETRIEVE');
        resseq.add(dataToXQV(data, uri, contenttype));
      end;
    end;
  end;

var v: IXQValue;
    i: SizeInt;
begin
  tempContext := defaultQueryEngine.getEvaluationContext();
  resseq := TXQValueSequence.create();
  result := resseq;
  for i := 1 to getSequenceCount do begin
    v := get(i);
    if v.kind = pvkNode then handle(resolveHTMLCallback(tempContext, 1, @V))
    else handle(v);
  end;
  xqvalueSeqSqueeze(result);
end;

function TXQValue.GetEnumerator: TXQValueEnumerator;
begin
  result.fguardian := self;
  result.ptr := GetEnumeratorPtrUnsafe;
end;

{$ImplicitExceptions off}

constructor TXQValueBoolean.create(abool: boolean);
begin
  inherited create(baseSchema.boolean);
  bool := abool;
end;

constructor TXQValueBoolean.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  bool := value.toBoolean;
end;

constructor TXQValueBoolean.create(atypeAnnotation: TXSType; abool: boolean);
begin
  ftypeAnnotation := atypeAnnotation;
  bool := abool;
end;

{$ImplicitExceptions on}

class function TXQValueBoolean.classKind: TXQValueKind;
begin
  result := pvkBoolean;
end;

function TXQValueBoolean.toBoolean: boolean;
begin
  result := bool;
end;

function TXQValueBoolean.toInt64: int64;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.toFloat: xqfloat;
begin
  if bool then result:=1 else result:=0;
end;

function TXQValueBoolean.toDecimal: bigdecimal;
begin
  if bool then setOne(result) else setZero(result);
end;

function TXQValueBoolean.toString: string;
begin
  if bool then result:='true' else result:='false';
end;

function TXQValueBoolean.hashCode: uint32;
begin
  Result:=ord(bool);
end;

function TXQValueBoolean.clone: IXQValue;
begin
  result:=xqvalue(bool);
end;

procedure TXQValueBoolean.jsonSerialize(var serializer: TXQSerializer);
begin
  if bool then serializer.append('true') else serializer.append('false');
end;

procedure TXQValueBoolean.adaptiveSerialize(var serializer: TXQSerializer);
begin
  if bool then serializer.append('true()')
  else serializer.append('false()')
end;


{$ImplicitExceptions off}
constructor TXQValueInt64.create(atypeAnnotation: TXSType);
begin
  inherited create(atypeAnnotation);
end;

constructor TXQValueInt64.create(const aint: int64);
begin
  inherited create(baseSchema.Integer);
  value := aint;
end;

constructor TXQValueInt64.create(atypeAnnotation: TXSType; const aint: int64);
begin
  ftypeAnnotation := atypeAnnotation;
  value := aint;
end;

constructor TXQValueInt64.create(atypeAnnotation: TXSType; const avalue: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  self.value := avalue.toInt64;
end;

{$ImplicitExceptions on}

class function TXQValueInt64.classKind: TXQValueKind;
begin
  Result := pvkInt64;
end;

function TXQValueInt64.toBoolean: boolean;
begin
  result:=value<>0;
end;

function TXQValueInt64.toInt64: int64;
begin
  result:=value;
end;

function TXQValueInt64.toDecimal: bigdecimal;
begin
  result:=value;
end;

function TXQValueInt64.toFloat: xqfloat;
begin
  Result:=value;
end;

function TXQValueInt64.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  ignore(scontext);
  result := value;
end;

function TXQValueInt64.toString: string;
begin
  result:=inttostr(value);
end;

function TXQValueInt64.toDateTime: TDateTime;
begin
  result:=extended(value);
end;

{$push}{$RangeChecks off}{$OverflowChecks off}
function TXQValueInt64.hashCode: uint32;
begin
  result := value mod THashInfo.modulus;
end;
{$pop}

procedure TXQValueInt64.jsonSerialize(var serializer: TXQSerializer);
begin
  serializer.appendNumber(value);
end;

procedure TXQValueInt64.adaptiveSerialize(var serializer: TXQSerializer);
begin
  if (typeAnnotation = baseSchema.integer) or (typeAnnotation = baseSchema.decimal) then begin
    serializer.appendNumber(value);
  end else begin
    serializer.appendTypeNameFunctionStart(typeAnnotation);
    serializer.appendXQueryString(toString);
    serializer.append(')');
  end;
end;

function TXQValueInt64.clone: IXQValue;
var
  temp: TXQValueInt64;
begin
  temp := TXQValueInt64Class(self.ClassType).Create(typeAnnotation);
  temp.value := value;
  result := temp;
end;

{$ImplicitExceptions off}
constructor TXQValueFloat.create(const aflt: xqfloat);
begin
  create(baseSchema.double, aflt);
end;

constructor TXQValueFloat.create(atypeannotation: TXSType; const aflt: xqfloat);
begin
  inherited create(atypeannotation);
  value := aflt;
end;

constructor TXQValueFloat.create(atypeAnnotation: TXSType; const avalue: IXQValue);
begin
  inherited create(atypeannotation);
  value := avalue.toFloat;
end;

class function TXQValueFloat.classKind: TXQValueKind;
begin
  Result:=pvkFloat;
end;
{$ImplicitExceptions on}


{class function TXQValueFloat.truncateRange(const v: BigDecimal): BigDecimal;
begin
  case (typeAnnotation as TXSNumericType).subType of
    xsstDouble: avalue := double(avalue);
    xsstFloat: avalue := single(avalue);
  end;
end;}

function TXQValueFloat.toBoolean: boolean;
begin
  Result:=not IsNan(value) and (value <> 0);
end;

function TXQValueFloat.toInt64: Int64;
begin
  Result:=trunc(value);
end;

function TXQValueFloat.toFloat: xqfloat;
begin
  Result:=value;
end;

function TXQValueFloat.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  ignore(scontext);
  Result:=value;
end;


function TXQValueFloat.toDecimal: BigDecimal;
begin
  try
    case (typeAnnotation as TXSNumericType).subType of
      xsstDouble: result := FloatToBigDecimal(double(value), bdffShortest);
      xsstFloat: result := FloatToBigDecimal(single(value), bdffShortest);
      else;
    end;
  except
    on EConvertError do raise EXQEvaluationException.Create('FOAR0002', 'Float ' + toXQuery()+  ' cannot be cast to decimal');
  end;
end;

function TXQValueFloat.toString: string;
  function doubleToString(const v: double): string;
  begin
    if ((v >= double(0.000001)) and (v < 1000000)) or ((v > -1000000) and (v <= double(-0.000001)))  then
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExact)
     else
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExponent);
  end;
  function singleToString(const v: single): string;
  begin
    if ((v >= single(0.000001)) and (v < 1000000)) or ((v > -1000000) and (v <= single(-0.000001)))  then
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExact)
     else
      result := BigDecimalToStr(FloatToBigDecimal(v, bdffShortest), bdfExponent);
  end;
begin
  if not value.isFinite() then begin
    case value.SpecialType of
      fsInf: exit('INF');
      fsNInf: exit('-INF');
      else exit('NaN');
    end;
  end;
  if value = 0 then
    if value.Sign then exit('-0')
    else exit('0');
  try
    case (typeAnnotation as TXSNumericType).subType of
      xsstDouble: result := doubleToString(double(value));
      xsstFloat: result := singleToString(single(value)); //this can overflow
      else;
    end;
  except
    on EConvertError do exit('internal float: '+FloatToStr(value));
    on EMathError do exit('internal float: '+FloatToStr(value));
  end;
end;

function TXQValueFloat.toDateTime: TDateTime;
begin
  Result:=nan;
end;

function TXQValueFloat.hashCode: uint32;
var
  m: QWord;
  e: Integer;
begin
  if not value.isFinite() then
    case value.SpecialType of
      fsInf: exit(THashInfo.hash_pos_inf);
      fsNInf: exit(THashInfo.hash_neg_inf);
      else exit(THashInfo.hash_nan);
    end;
  result := 0;
  m := value.Mantissa;
  e := value.Exponent - 21; //it only works with -21 ??
  result := m mod THashInfo.modulus;

  //see pyhash
  if e >= 0 then e := e mod THashInfo.modulus_bits
  else e := THashInfo.modulus_bits - 1 - ( (-1 - e) mod THashInfo.modulus_bits );
  result := ( (result shl e) and THashInfo.modulus) or (result shr ( THashInfo.modulus_bits - e ));

  if value.Sign then result := cardinal.MaxValue - result + 1;
end;

procedure TXQValueFloat.jsonSerialize(var serializer: TXQSerializer);
begin
  if value.isFinite() then begin
    if value = 0 then begin
      if value.Sign then serializer.append('-0')
      else serializer.append('0');
    end else if frac(value) = 0 then begin
      serializer.append(IntToStr(trunc(value)));
    end else
      serializer.append(toString);
  end else begin
    if serializer.standard then serializer.error('SERE0020', self);
    case value.SpecialType of
      fsInf: serializer.appendJSONString('INF');
      fsNInf: serializer.appendJSONString('-INF');
      else serializer.append('NaN');
    end;
  end;
end;

procedure TXQValueFloat.adaptiveSerialize(var serializer: TXQSerializer);
var s: shortstring;
  dot, e, i, cutoff: Integer;
begin
  if typeAnnotation <> baseSchema.double then begin
    inherited adaptiveSerialize(serializer);
    exit
  end;

  //if withType then serializer.appendTypeNameFunctionStart(typeAnnotation);
  if value.isFinite() then begin
    system.str(value, s);
    //trim
    cutoff := 0;
    while s[cutoff + 1] = ' ' do inc(cutoff);
    delete(s, 1, cutoff);
    //convert x.y0000E+0000z to x.yez
    dot := 0;
    e := length(s) + 1;
    for i := 1 to length(s) do
      case s[i] of
        'E': begin
          s[i] := 'e';
          if s[i + 1] = '+' then delete(s, i + 1, 1);
          while (i + 1 < length(s)) and (s[i + 1] = '0') do delete(s, i + 1, 1);
          e := i;
          break;
        end;
        '.': dot := i;
      end;
    cutoff := 0;
    if dot > 0 then while (e - cutoff - 1 > dot + 1) and (s[e - cutoff - 1] = '0') do inc(cutoff);
    if cutoff > 0 then delete(s, e - cutoff, cutoff);
    serializer.append(s);
  end else begin
    case value.SpecialType of
      fsInf: serializer.append('INF');
      fsNInf: serializer.append('-INF');
      else serializer.append('NaN');
    end;
  end;
end;

function TXQValueFloat.clone: IXQValue;
begin
  Result:=TXQValueFloat.create(typeAnnotation, value);
end;

{$ImplicitExceptions off}
constructor TXQValueDecimal.create(const v: BigDecimal);
begin
  if isIntegral(v) then inherited create(baseSchema.integer)
  else inherited create(baseSchema.decimal);
  value := v;
end;

constructor TXQValueDecimal.create(atypeannotation: TXSType; const v: BigDecimal);
begin
  inherited create(atypeannotation);
  value := v;
end;

constructor TXQValueDecimal.create(atypeAnnotation: TXSType; const avalue: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  value := avalue.toDecimal;
end;

destructor TXQValueDecimal.Destroy;
begin
  value.digits := nil;
  inherited Destroy;
end;

{$ImplicitExceptions on}

class function TXQValueDecimal.classKind: TXQValueKind;
begin
  Result:=pvkBigDecimal;
end;


function TXQValueDecimal.toBoolean: boolean;
begin
 Result:=(value <> 0);
end;

function TXQValueDecimal.toInt64: Int64;
begin
  result := BigDecimalToInt64(value);
end;

function TXQValueDecimal.toDecimal: BigDecimal;
begin
 Result:=value;
end;

function TXQValueDecimal.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  ignore(scontext);
  result := toFloat;
end;

function TXQValueDecimal.toString: string;
begin
  result := BigDecimalToStr(value);
end;

function TXQValueDecimal.toDateTime: TDateTime;
begin
 Result:= double.NaN;
end;

function TXQValueDecimal.hashCode: uint32;
var r, digit: BigDecimalBinSquared;
  i: sizeint;
begin
  r := 0;
  digit := 1;
  for i := 0 to high(value.digits) do begin
    r := r + digit * value.digits[i] ;
    if r >= THashInfo.modulus then r := r mod thashinfo.modulus;
    digit := (digit * ELEMENT_OVERFLOW) mod THashInfo.modulus;
  end;
  if value.exponent > 0 then r := r * modPow(ELEMENT_OVERFLOW, value.exponent, thashinfo.modulus)
  else if value.exponent < 0 then begin
    assert((ELEMENT_OVERFLOW = 1000000000) and (thashinfo.bigdecimalInverse = 968029830));
    r := r * modPow(THashInfo.bigdecimalInverse, -value.exponent, thashinfo.modulus);
  end;
  if r >= THashInfo.modulus then r := r mod thashinfo.modulus;
  result := r;
  if value.signed then result := uint32.MaxValue - result + 1;
end;

procedure TXQValueDecimal.jsonSerialize(var serializer: TXQSerializer);
begin
  serializer.append(toString);
end;

procedure TXQValueDecimal.adaptiveSerialize(var serializer: TXQSerializer);
begin
  if (typeAnnotation = baseSchema.integer) or (typeAnnotation = baseSchema.decimal) then begin
    serializer.append(toString);
  end else begin
    serializer.appendTypeNameFunctionStart(typeAnnotation);
    serializer.appendXQueryString(toString);
    serializer.append(')');
  end;
end;

function TXQValueDecimal.clone: IXQValue;
begin
  result := TXQValueDecimalClass(self.ClassType).Create(ftypeAnnotation, value);
end;

{$ImplicitExceptions off}
constructor TXQValueString.create(const astr: string);
begin
  inherited create(baseSchema.string_);
  str := astr;
end;

constructor TXQValueString.create(atypeAnnotation: TXSType; const astr: string);
begin
  ftypeAnnotation := atypeAnnotation;
  str := astr;
end;

constructor TXQValueString.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  ftypeAnnotation := atypeAnnotation;
  str := value.toString;
end;

destructor TXQValueString.Destroy;
begin
  str := '';
  inherited Destroy;
end;

{$ImplicitExceptions on}

class function TXQValueString.classKind: TXQValueKind;
begin
  Result:=pvkString;
end;

function TXQValueString.toBoolean: boolean;
begin
  Result:= (str <> '') and (str <> '0') and (str <> 'false');
end;

function TXQValueString.toBooleanEffective: boolean;
begin
  if ftypeAnnotation.derivedFrom([baseSchema.string_, baseSchema.untypedAtomic, baseSchema.anyURI]) then
    result := length(str) > 0
   else
    raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to boolean.');
end;

function TXQValueString.toString: string;
begin
  result := str;
end;

function TXQValueString.toDateTime: TDateTime;
begin
  result := StrToDateTimeDef(str,0);
end;

function TXQValueString.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  if scontext.strictTypeChecking and not instanceOf(baseSchema.untypedAtomic) then raiseXPTY0004TypeError(self, 'float/double');
  result := xqfloat.parse(str);
  if result.IsNan() and (trim(str) <> 'NaN') then raiseFORG0001InvalidConversion(self, 'float/double')
end;

function TXQValueString.hashCode: uint32;
begin
  Result:=TXQDefaultTypeInfo.hash(pchar(str), length(str));
end;


function TXQValueString.clone: IXQValue;
begin
  result := TXQValueString.create(ftypeAnnotation, str);
end;





constructor TXQValueBinary.create(atypeAnnotation: TXSType; const astr: string);
begin
  inherited create(atypeAnnotation);
  encoded := astr;
end;

destructor TXQValueBinary.Destroy;
begin
  encoded := '';
  inherited Destroy;
end;

class function TXQValueBinary.classKind: TXQValueKind;
begin
  Result:=pvkBinary;
end;

function TXQValueBinary.toBoolean: boolean;
begin
  Result:= (encoded <> '');
end;

function TXQValueBinary.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to boolean.');
  result := false;
end;

function TXQValueBinary.toString: string;
begin
  Result:=encoded;
end;

function TXQValueBinary.toDateTime: TDateTime;
begin
  raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to datetime.');
  result := 0;
end;

function TXQValueBinary.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  raise EXQEvaluationException.create('FORG0006', 'Cannot convert '+toXQuery() + ' to float.');
  result := 0;
end;

function TXQValueBinary.hashCode: uint32;
begin
  Result:=TXQDefaultTypeInfo.hash(pchar(encoded), length(encoded));
end;

function base64ToBytes(const s:string):TBytes;

var
  ss: tstringstream;
  decoder: TBase64DecodingStream = nil;
begin
  result := nil;
  if Length(s) = 0 then exit;
  ss := TStringStream.Create(s);
  try
    decoder := TBase64DecodingStream.Create(ss, bdmStrict);
    setlength(Result, decoder.size);
    decoder.Read(result[0], decoder.Size);
  finally
    decoder.free;
    ss.Free;
  end;
end;


function TXQValueBinary.toBinaryBytes: TBytes;
begin
  if instanceOf(baseSchema.hexBinary) then result := encoded.DecodeHexToBytes
  else if instanceOf(baseSchema.base64Binary) then result := base64ToBytes(encoded)
  else begin assert(false); result := nil; end;
end;


class function TXQValueBinary.compare(a, b: TXQValue): TXQCompareResult;
var binaries: array[0..1] of tbytes = (nil, nil);
    temp: string;
    untypedIndex: integer;
    at, bt: (tBase64, tHex, tUntyped, tOther);
begin
  if a.typeAnnotation <> b.typeAnnotation then begin
    if a.instanceOf(baseSchema.base64Binary) then at := tBase64
    else if a.instanceOf(baseSchema.hexBinary) then at := tHex
    else if a.instanceOf(baseSchema.untypedAtomic) then at := tUntyped
    else at := tOther;
    if b.instanceOf(baseSchema.base64Binary) then bt := tBase64
    else if b.instanceOf(baseSchema.hexBinary) then bt := tHex
    else if b.instanceOf(baseSchema.untypedAtomic) then bt := tUntyped
    else bt := tOther;

    result := xqcrEqual;
    if (at = tUntyped) or (bt = tUntyped) then begin
      //this branch is only taken if strict-type-checking is disabled and it is compared using eq/ne/..  ?? todo:can this be simplified
      try
        if at = tUntyped then begin
          untypedIndex := 0;
          temp := a.toString;
          binaries[1] := b.toBinaryBytes;
        end else begin
          untypedIndex := 1;
          binaries[0] := a.toBinaryBytes;
          temp := b.toString
        end;
        if (at = tBase64) or (bt = tBase64) then binaries[untypedIndex] := base64ToBytes(temp)
        else begin
          if length(temp) and 1 = 1 then exit(xqcrIncomparable);
          binaries[untypedIndex] := temp.DecodeHexToBytes;
        end;
      except
        on e: Exception do  {StreamError for base64, exception for hex}
          exit(xqcrIncomparable);
      end;
    end else if ((at = tHex) and (bt = tHex)) or ((at = tBase64) and (bt = tBase64)) then begin
      binaries[0] := a.toBinaryBytes;
      binaries[1] := b.toBinaryBytes;
    end else begin
      //if strictTypeChecking then raiseXPTY0004TypeError(a, 'binary like ' + b.toXQuery);
      exit(xqcrIncomparable);
    end;
  end else begin
    binaries[0] := a.toBinaryBytes;
    binaries[1] := b.toBinaryBytes;
  end;
  result := TXQCompareResult.compare(binaries[0], binaries[1]);
  //todo: less-than/greater-than should raise exception unless 3.1 mode is enabled
end;
function TXQValueBinary.clone: IXQValue;
begin
  Result:=TXQValueBinary.create(typeAnnotation, encoded);
end;







constructor TXQValueQName.create(atypeAnnotation: TXSType; const aurl, aprefix, alocal: string);
begin
  inherited create(atypeAnnotation);
  prefix := aprefix;
  url := aurl;
  local := alocal;

end;

constructor TXQValueQName.create(atypeAnnotation: TXSType; const ns: TNamespace; const alocal: string);
begin
  if ns = nil then create(atypeAnnotation, '', '', alocal)
  else create(atypeAnnotation, ns.getURL, ns.getPrefix, alocal);
end;

constructor TXQValueQName.create(const aurl, aprefix, alocal: string);
begin
  create(baseSchema.QName, aurl, aprefix, alocal);
end;

constructor TXQValueQName.create(const aurl, aprefixedLocal: string);
var
  i: SizeInt;
begin
  i := pos(':', aprefixedLocal);
  if i = 0 then create(aurl, '', aprefixedLocal)
  else create(aurl, copy(aprefixedLocal, 1,i-1), strCopyFrom(aprefixedLocal,i+1));
end;

constructor TXQValueQName.create(const ns: TNamespace; const alocal: string);
begin
  if ns = nil then create(baseSchema.QName, '', '', alocal)
  else create(baseSchema.QName, ns.getURL, ns.getPrefix, alocal);
end;

constructor TXQValueQName.create(atypeAnnotation: TXSType; const value: IXQValue);
begin
  inherited create(atypeAnnotation);
  if value is TXQValueQName then begin
    prefix := (value as TXQValueQName).prefix;
    url := (value as TXQValueQName).url;
    local := (value as TXQValueQName).local;
  end else raise EXQEvaluationException.create('XPTY0004', 'Cannnot cast '+value.toXQuery()+' to xs:QName');
end;

destructor TXQValueQName.Destroy;
begin
  prefix := '';
  url := '';
  local := '';
  inherited Destroy;
end;

class function TXQValueQName.classKind: TXQValueKind;
begin
  Result:=pvkQName;
end;

function TXQValueQName.toString: string;
begin
  if prefix <> '' then result := prefix + ':' + local
  else result := local;
end;

function TXQValueQName.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.Create('FORG0006', 'A QName cannot be used a boolean');
  result := false;
end;

function TXQValueQName.hashCode: uint32;
begin
  Result:=TXQDefaultTypeInfo.hash(pchar(url), length(url)) xor TXQDefaultTypeInfo.hash(pchar(local), length(local));
end;

procedure TXQValueQName.adaptiveSerialize(var serializer: TXQSerializer);
begin
  serializer.appendQualifiedQName(url, local);
end;

function TXQValueQName.clone: IXQValue;
begin
  Result:=TXQValueQName.create(ftypeAnnotation, url, prefix, local);
end;
{ TXQValueDateTime }

constructor TXQValueDateTime.create(atypeAnnotation: TXSType);
begin
  inherited create(atypeAnnotation);
  fillchar(value, sizeof(value), 0);
  value.timezone:=high(integer);
end;

constructor TXQValueDateTime.create(atypeAnnotation: TXSDateTimeType; const str: string);
var
  res: TDateTimeParsingResult;
begin
  res := tryCreateFromString(str,  atypeAnnotation.fixedDateTimePattern, @value);
  if res <> dtprSuccess then
    raise EXQEvaluationException.Create(IfThen(res = dtprFailureValueTooHigh, 'FODT0001', 'FORG0001' ), 'Invalid conversion from '+str+' to date format ' + atypeAnnotation.fixedDateTimePattern);
  ftypeAnnotation := atypeAnnotation;
end;


constructor TXQValueDateTime.create(atypeAnnotation: TXSType; const str, format: string);
var
  res: TDateTimeParsingResult;
begin
  res := tryCreateFromString(str, format, @value);
  if res <> dtprSuccess then
    raise EXQEvaluationException.Create(IfThen(res = dtprFailureValueTooHigh, 'FODT0001', 'FORG0001' ), 'Invalid conversion from '+str+' to date format ' + format);
  ftypeAnnotation := atypeAnnotation;
end;

constructor TXQValueDateTime.create(atypeAnnotation: TXSType; const dt: TXQValueDateTimeData);
begin
  ftypeAnnotation := atypeAnnotation;
  value := dt;
end;

constructor TXQValueDateTime.create(atypeAnnotation: TXSType; const dt: TDateTime);
begin
  ftypeAnnotation := atypeAnnotation;
  setDateTime(dt);
end;

class function TXQValueDateTime.classKind: TXQValueKind;
begin
  Result:=pvkDateTime;
end;

function TXQValueDateTime.toBoolean: boolean;
begin
 Result:=toDateTime <> 0;
end;

function TXQValueDateTime.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.Create('FORG0006', 'A datetime cannot be used a boolean');
  Result := false;
end;

function TXQValueDateTime.toInt64: Int64;
begin
 Result:=trunc(toDateTime);
end;

function TXQValueDateTime.toDecimal: BigDecimal;
begin
 Result:=toDateTime;
end;

function TXQValueDateTime.toString: string;
var
  fac: Integer;
  i: Integer;
  v: TXQValueDateTimeData;
begin
  if typeAnnotation = baseSchema.dateTime then
    result := bbutils.dateTimeFormat('yyyy-mm-ddThh:nn:ss[.z+][Z]', value.year, value.month, value.day, value.hour, value.min, value.seconds, value.microsecs * 1000, value.timezone)
  else begin
    fac := 1;
    if (typeAnnotation as TXSDateTimeType).isDuration then begin
      for i:=1 to 7 do  if (value.values[i] <> high(integer)) and (value.values[i] < 0) then begin fac:= -1; break; end;
      v := value;
      setDayTime(v, v.toDayTime);
      setMonths(v, v.toMonths, true);
    end else v := value;
    result := bbutils.dateTimeFormat((typeAnnotation as TXSDateTimeType).fixedDateTimePattern, fac * v.year, fac * v.month, fac * v.day, fac * v.hour, fac * v.min, fac * v.seconds, fac * v.microsecs * 1000, v.timezone);
    if (fac < 0) and (result <> 'P') then result := '-' + result;
  end;
  if (result = 'P') then begin
    if ftypeAnnotation.derivedFrom(baseSchema.yearMonthDuration) then
      result := 'P0M'
     else
      result := 'PT0S';
  end;
end;

function TXQValueDateTime.toDateTime: TDateTime;
var
  tempsecs: TDateTime;
begin
 if not dateEncodeTry(value.year, value.month, value.day, result) then
   raise EXQEvaluationException.Create('FORG0006', 'Invalid date: '+IntToStr(value.year)+'-'+IntToStr(value.month)+'-'+IntToStr(value.day)+ ' (probably tried to use duration as datetime)');
 tempsecs := (value.hour * 3600 + value.min *60 + value.seconds);
 if value.microsecs <> high(integer) then tempsecs += value.microsecs / MicroSecsPerSec;
 if value.timezone <> high(integer) then tempsecs -= value.timezone * 60 ;
 result += tempsecs / SecsPerDay;
end;

function TXQValueDateTime.getInternalDateTimeData: PXQValueDateTimeData;
begin
  result := @value;
end;

{$push}{$RangeChecks off}{$OverflowChecks off}
function TXQValueDateTime.hashCode: uint32;
begin
 with value do
 result :=   ((((year shl 4) or month) shl 5) or day)
         xor ((((hour shl 6) or (min + timezone) ) shl 6) or seconds)
         xor timezone;
end;
{$pop}

procedure TXQValueDateTime.setDateTime(const dateTime: TDateTime);
var
  y,m,d:integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  value.year:=y;
  value.month:=m;
  value.day:=d;
  value.hour:=h;
  value.min:=n;
  value.seconds:=s;
  value.microsecs:=integer(ms)*1000;
  value.timezone:=high(integer);
  truncateRange;
end;

class procedure TXQValueDateTime.setDateTime(const dateTime: TDateTime; out v: TXQValueDateTimeData);
var
  y,m,d:integer;
  h,n,s,ms: word;
begin
  dateDecode(dateTime, @y, @m, @d);
  DecodeTime(dateTime, h, n, s, ms);
  v.year:=y;
  v.month:=m;
  v.day:=d;
  v.hour:=h;
  v.min:=n;
  v.seconds:=s;
  v.microsecs:=ms*1000;
  v.timezone:=high(integer);
end;

function TXQValueDateTime.clone: IXQValue;
begin
  result := TXQValueDateTimeClass(self.ClassType).Create(typeAnnotation, value);
end;

class function TXQValueDateTime.tryCreateFromString(const s, format: string; data: PXQValueDateTimeData): TDateTimeParsingResult;
var
  tempData: TXQValueDateTimeData;
  formats: TStringArray;
  usedFormat: string = '';
  i: Integer;
  duration: Boolean;
  j: Integer;
  compMax: LongInt;

  function componentNonZero(c: integer): boolean;
  begin
    result := (c <> high(integer)) and (c > 0);
  end;

const componentMax: array[1..6] of integer = (high(integer), 13, 32, 25, 60, 60);
const componentFiller: array[1..6] of integer = (1972, 12, 31, 0, 0, 0);
const componentChars: string = 'ymdhns';
begin
  result := dtprFailure;
  if data = nil then data := @tempData;
  duration := strBeginsWith(format, '[-]P');
  if duration and ((length(s) <= 2) or strEndsWith(s, 'T')) then exit;
  formats := strSplit(format, '|');
  if length(formats) = 0 then exit;
  for i:=0 to high(formats) do begin
    usedFormat:=formats[i];
    result := dateTimeParsePartsTry(s, usedFormat, @data^.year, @data^.month, @data^.day, @data^.hour, @data^.min, @data^.seconds, @data^.microsecs, @data^.timezone, [dtpfStrict]);
    if result = dtprSuccess then begin
      if ( (not duration) and (
             ((data^.year = 0) and (baseSchema.version = xsd10))
            or (((abs(data^.year) < 10000) and strBeginsWith(usedFormat, 'y')
                  and not strBeginsWith(s, strFromInt(data^.year, 4))
                  and not ((data^.year = 0) and strBeginsWith(s, '-0000'))  ))))
         or ((data^.hour = 24) and (componentNonZero(data^.min) or componentNonZero(data^.seconds) or componentNonZero(data^.microsecs)))
         or ((data^.timezone <> high(Integer)) and ((data^.timezone < -14 * 60) or (data^.timezone > 14 * 60)))
         then
        result := dtprFailure;
    end;
    if result <> dtprSuccess then continue;
    if data^.microsecs = high(data^.microsecs) then data^.microsecs := 0
    else if data^.microsecs <> 0 then data^.microsecs := (data^.microsecs + 500) div 1000;
    if duration and strBeginsWith(s, '-') then begin
      for j:=low(data^.values)  to high(data^.values) do if data^.values[j] <> high(integer) then data^.values[j] := - data^.values[j];
    end;
    break;
  end;
  if (result = dtprFailureValueTooHigh) and duration then result := dtprFailureValueTooHigh2;
  if result <> dtprSuccess then
    exit();
  if not duration then begin
    for i:=1 to 6 do
      if (i in [2,3]) and (data^.values[i] = 0) then begin
        if pos(componentChars[i], usedFormat) > 0 then exit(dtprFailure);
      end else begin
        compMax := componentMax[i];
        if (i = 3) and componentNonZero(data^.month) then compMax := MonthDays[dateIsLeapYear(data^.year), data^.month]+ 1;
        if data^.values[i] >= compMax then begin
          if pos(componentChars[i], usedFormat) > 0 then exit(dtprFailure);
          data^.values[i] := componentFiller[i];
        end;
      end;
    if data = @tempData then exit;
    if data^.hour = 24 then begin
      data^.hour:=0;
      data^.day+=1;
      if data^.day > MonthDays[dateIsLeapYear(data^.year), data^.month] then begin
        data^.day:=1;
        data^.month+=1;
        if data^.month > 12 then begin
          data^.month:=1;
          data^.year+=1;
          if data^.year = 0 then data^.year+=1;
        end;
      end;
    end;
  end else begin
    for i:=1 to 6 do if data^.values[i] >= high(integer) then begin
      //if pos(componentChars[i], usedFormat) > 0 then exit(false);
      data^.values[i] := 0;
    end;
  end;
end;

function fquotient(a, b: integer): integer; inline; //= floor (a/b)
begin
  result := a div b;
  if a < 0 then begin
    if result * b = a then exit;
    result -= 1;
  end
end;



class procedure TXQValueDateTime.addDurationDToDateS(const S, D: TXQValueDateTimeData; out E: TXQValueDateTimeData);
begin
  E.timezone:=S.timezone;

  E.month:=S.toMonths  + d.toMonths;
  if E.month > 12 then setMonths(E, E.month,false) //the last month A.D. is 13 ( = 01.01.0001)
  else begin
    setMonths(E, 25 - E.month, false);             //the first month B.C. is also "13" ( like 01.01|12.-0001)
    E.month:= 13 - E.month;                        //but now the months are running backwards, so invert from december <-> january
    E.year:=-E.year;
    if baseSchema.version = xsd11 then inc(E.year);
  end;

  E.microsecs := S.microsecs + D.microsecs;
  E.seconds  := fquotient(E.microsecs, MicroSecsPerSec); E.microsecs := E.microsecs - E.seconds * MicroSecsPerSec;
  E.seconds  := S.seconds + D.seconds + E.seconds;
  E.min      := fquotient(e.seconds, 60);  E.seconds := E.seconds - E.min * 60;
  E.min      := S.min + D.min + E.min;
  E.hour     := fquotient(e.min, 60);  E.min := E.min - E.hour * 60;
  E.hour     := S.hour + D.hour + E.hour;
  E.day      := fquotient(E.hour, 24); E.hour:= E.hour - E.day * 24;
  E.day      := D.day + E.day + intBound(1, S.day, MonthDays[dateIsLeapYear(E.year), E.month]);

  //official w3c algorithm (http://www.w3.org/TR/xmlschema-2/#adding-durations-to-dateTimes, todo: optimize), except that maximumDayInMonthFor(E[year], E[month] - 1) in their pseudo code is undefined for january!
  while (e.day < 1) or (e.day > MonthDays[dateIsLeapYear(e.year), e.month]) do begin
    if e.day < 1 then begin
      e.month-=1;
      if e.month <= 0 then begin
        e.month:=12;
        e.year-=1;
        if (e.year = 0) and (baseSchema.version = xsd10) then e.year -= 1;
      end;
      e.day := e.day + MonthDays[dateIsLeapYear(e.year), e.month];
    end else begin
      e.day := e.day - MonthDays[dateIsLeapYear(e.year), e.month];
      e.month+=1;
      if e.month > 12 then begin
        e.month:=1;
        e.year+=1;
      end;
    end;
  end;

end;

class procedure TXQValueDateTime.setMonths(var duration: TXQValueDateTimeData; m: integer; isDuration: boolean);
var neg: boolean;
begin
  if m = 0 then begin duration.month:=0;  duration.year:=0; exit; end;
  neg := m < 0; m := abs(m);
  duration.month := m;
  duration.year := fquotient(duration.month - 1, 12);
  duration.month := duration.month - duration.year * 12;
  if neg then begin duration.month:=-duration.month;  duration.year:= -duration.year;end;
  if isDuration and (abs(duration.month) = 12) then begin
    if neg then duration.year-=1
    else duration.year+=1;
    duration.month:=0;
  end;
end;

class procedure TXQValueDateTime.setDayTime(var duration: TXQValueDateTimeData; dt: int64);
begin
  {duration.microsecs:=round(frac(dt) * MicroSecsPerSec);
  dti := trunc(dt);
  if abs(duration.secfraction) < 0.000001 then duration.secfraction:=0;
  if dt > 0 then begin
    if duration.secfraction > 0.999999 then begin duration.secfraction:=0; dti+=1; end;
  end else if dt < 0 then begin
    if duration.secfraction < -0.999999 then begin duration.secfraction:=0; dti-=1; end;
  end;}

  duration.day       := dt  div (24*60*60*MicroSecsPerSec); dt := dt mod (24*60*60*MicroSecsPerSec);
  duration.hour      := dt  div (60*60*MicroSecsPerSec);    dt := dt mod (60*60*MicroSecsPerSec);
  duration.min       := dt  div (60*MicroSecsPerSec);       dt := dt mod (60*MicroSecsPerSec);
  duration.seconds   := dt  div (MicroSecsPerSec);          dt := dt mod (MicroSecsPerSec);
  duration.microsecs := dt;
end;

procedure TXQValueDateTime.truncateRange();
begin
  case (ftypeAnnotation as TXSDateTimeType).truncation of
    xqdttDate:      begin value.year := 1989; value.month := 12; value.day := 30; end;
    xqdttTime:      begin value.hour := 0; value.min := 0; value.seconds := 0; value.microsecs := 0; end;
    xqdttYearMonth: begin value.year := 0; value.month := 0;   end;
    xqdttNone:;
  end;
end;

class function TXQValueDateTime.compare(const a, b: TXQValueDateTime; implicitTimezone: integer): TXQCompareResult;
const formatIds: string = 'ymdhns';
const componentFiller: array[1..6] of integer = (1972, 1, 1, 0, 0, 0);
var
  adf, bdf: String;
  av, bv: TXQValueDateTimeData;
  i: Integer;

  overlap: Integer;
begin
  result := xqcrEqual;
  adf := lowercase((a.typeAnnotation as TXSDateTimeType).fixedDateTimePattern);
  bdf := lowercase((b.typeAnnotation as TXSDateTimeType).fixedDateTimePattern);
  av := a.value;
  bv := b.value;
  //replace unimportant values with reference date
  overlap := 6;
  for i := 1 to 6 do begin
    if (pos(formatIds[i], adf) > 0) and (pos(formatIds[i], bdf) > 0) then continue;
    av.values[i] := componentFiller[i];
    bv.values[i] := componentFiller[i];
    overlap -= 1;
    {if (i = 3) and (av.month=2) and (bv.month=2)  then begin
      av.day:=1;
      bv.day:=1;
    end;}
  end;
  if overlap = 0 then exit(xqcrIncomparable); //not comparable

  //carefully here, the timezone difference might change every date component, even the year
  if av.timezone = high(Integer) then av.timezone := implicitTimezone;
  if bv.timezone = high(Integer) then bv.timezone := implicitTimezone;
  result := TXQCompareResult.compare(av.toMicroSecondStamp(), bv.toMicroSecondStamp());
end;


{$ImplicitExceptions off}

constructor TXQValueSequence.create(capacity: SizeInt);
begin
  inherited create(baseSchema.sequence);
  seq := TXQVList.Create(capacity);
end;

constructor TXQValueSequence.create(firstChild: IXQValue);
begin
  inherited create(baseSchema.sequence);
  seq := TXQVList.Create;
  seq.add(firstChild);
end;

constructor TXQValueSequence.create(list: TXQVList);
begin
  inherited create(baseSchema.sequence);
  seq := list;
end;

{$ImplicitExceptions on}

class function TXQValueSequence.classKind: TXQValueKind;
begin
  Result:=pvkSequence;
end;

function TXQValueSequence.isUndefined: boolean;
begin
  Result:=seq.Count=0;
end;

function TXQValueSequence.toBoolean: boolean;
begin
  if seq.Count >= 1 then result := seq[0].toBoolean
  else result:=false;
end;

function TXQValueSequence.toBooleanEffective: boolean;
begin
  case getSequenceCount of
    0: exit(false);
    1: exit(seq[0].toBooleanEffective);
    else
      case seq[0].kind of
        pvkNode, pvkObject, pvkArray: exit(true);
        else raise EXQEvaluationException.Create('FORG0006', 'A sequence starting with an atomic value cannot be used as boolean', nil, self);
      end;

  end;
end;

function TXQValueSequence.toInt64: Int64;
begin
  if seq.Count >= 1 then result := seq[0].toInt64
  else result := 0;
end;

function TXQValueSequence.toDecimal: BigDecimal;
begin
  if seq.Count >= 1 then result := seq[0].toDecimal
  else result := 0;
end;

function TXQValueSequence.toFloatChecked(scontext: TXQStaticContext): xqfloat;
begin
  if getSequenceCount <> 1 then raiseXPTY0004TypeError(self, 'float/double');
  Result := seq[0].toFloatChecked(scontext);
end;

{$ImplicitExceptions off}
function TXQValueSequence.toString: string;
begin
  result := toJoinedString('')
end;
{$ImplicitExceptions on}

function TXQValueSequence.toJoinedString(const sep: string): string;
var i: SizeInt;
    builder: TStrBuilder;
begin
  if seq.count = 0 then exit('');
  builder.init(@result);
  builder.append(seq[0].toJoinedString(sep));
  for i := 1 to seq.count - 1 do begin
    builder.append(sep);
    builder.append(seq[i].toJoinedString(sep));
  end;
  builder.final;
end;

function TXQValueSequence.toDateTime: TDateTime;
begin
  if seq.Count >= 1 then result := seq[0].toDateTime
  else result := 0;
end;

{$ImplicitExceptions off}
function TXQValueSequence.toNode: TTreeNode;
begin
  if seq.Count >= 1 then result := seq[0].toNode
  else result := nil;
end;

function TXQValueSequence.hashCode: uint32;
var
  pv: PIXQValue;
begin
  //it is only supposed to calculate hashs of atomic values
  case getSequenceCount of
    0: result := 0;
    1: result := seq[0].hashCode;
    else begin
      result := 0;
      for pv in GetEnumeratorPtrUnsafe do
        result := result xor pv^.hashCode;
    end;
  end;
end;

function TXQValueSequence.toArray: TXQVArray;
var
  i: SizeInt;
begin
  result := nil;
  setlength(result, seq.Count);
  for i:=0 to high(result) do result[i] := seq[i];
end;

function TXQValueSequence.toXQVList: TXQVList;
var
  i: SizeInt;
begin
  result := TXQVList.Create;
  for i:=0 to seq.Count-1 do
    result.add(seq[i]);
end;

function TXQValueSequence.getSequenceCount: SizeInt;
begin
  Result:=seq.Count;
end;


function TXQValueSequence.get(i: SizeInt): IXQValue;
begin
  if (i < 1) or (i > seq.Count) then exit(xqvalue);
  exit(seq[i-1]);
end;
{$ImplicitExceptions on}

function TXQValueSequence.GetEnumeratorPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  if seq.Count > 0 then begin
    result.fcurrent:=@seq.fbuffer[0];
    dec(result.fcurrent);
    result.flast := @seq.fbuffer[seq.Count-1];
    //we do not need to set result.fsingleelement, since we set flast to non-nil
  end else TXQValueEnumeratorPtrUnsafe.clear(result);
end;

function TXQValueSequence.map(const q: string): IXQValue;
var
  term: TXQuery;
  resseq: TXQValueSequence;
  i: SizeInt;
begin
  term := defaultQueryEngine.parseTerm(q, xqpmXQuery3_1, defaultQueryEngine.StaticContext);
  try
    resseq := TXQValueSequence.create(getSequenceCount);
    for i := 0 to seq.Count - 1 do
      resseq.add(term.evaluate(seq[i]));
    result := resseq;
    xqvalueSeqSqueeze(result);
  finally
    term.free;
  end;
end;

function TXQValueSequence.order(const q: string): IXQValue;
begin
  result := self.query('for $_ in $_ order by ' + q + ' return $_');
end;

function TXQValueSequence.clone: IXQValue;
var
  i: SizeInt;
  seqr: TXQValueSequence;
begin
  seqr := TXQValueSequence.Create(seq.Count);
  for i:=0 to seq.Count-1 do
    seqr.seq.Add(seq[i].clone);
  result := seqr;
end;

procedure TXQValueSequence.jsonSerialize(var serializer: TXQSerializer);
var
  i: SizeInt;
  ws: TXQSerializerInsertWhitespace;
begin
  case seq.Count of
    0: serializer.append('null');
    1: seq[0].jsonSerialize(serializer);
    else begin
      if serializer.standard then serializer.error('SERE0023', self);
      ws := serializer.insertWhitespace;
      if (ws = xqsiwIndent) and (seq.Count <= 8) then begin
        serializer.insertWhitespace := xqsiwConservative;
        for i := 0 to seq.Count - 1 do
          if seq[i].kind in [pvkObject,pvkArray] then begin
            serializer.insertWhitespace := xqsiwIndent;
            break;
          end;
      end;
      with serializer do begin
        appendJSONArrayStart;
        seq[0].jsonSerialize(serializer);
        for i := 1 to seq.Count-1 do begin
          appendJSONArrayComma();
          seq[i].jsonSerialize(serializer);
        end;
        appendJSONArrayEnd;
      end;
      serializer.insertWhitespace := ws;
    end;
  end;
end;

procedure TXQValueSequence.xmlSerialize(var serializer: TXQSerializer);
var
  i: SizeInt;
begin
  with serializer do
    if seq.Count = 0 then
      appendXMLEmptyElement(sequenceTag)
     else begin
       appendXMLElementStartTag(sequenceTag);
       appendXMLElementStartTag(elementTag);
       seq[0].xmlSerialize(serializer);
       appendXMLElementEndTag(elementTag);
       for i := 1 to seq.Count-1 do begin
         appendXMLElementStartTag(elementTag);
         seq[i].xmlSerialize(serializer);
         appendXMLElementEndTag(elementTag);
       end;
       appendXMLElementEndTag(sequenceTag);
     end;
end;

procedure TXQValueSequence.adaptiveSerialize(var serializer: TXQSerializer);
var
  i: SizeInt;
begin
  case getSequenceCount of
    0: serializer.append('()');
    1: seq[0].adaptiveSerialize(serializer);
    else begin
      serializer.append('(');
      seq[0].adaptiveSerialize(serializer);
      for i := 1 to getSequenceCount - 1 do begin
        serializer.append(',');
        seq[i].adaptiveSerialize(serializer);
      end;
      serializer.append(')');
    end;
  end;
end;

function TXQValueSequence.stringifyNodes: IXQValue;
begin
  result := TXQValueSequence.create(seq.stringifyNodes);
end;

function TXQValueSequence.hasNodes: boolean;
begin
  result := seq.hasNodes;
end;

procedure TXQValueSequence.addOrdered(const node: IXQValue);
begin
  seq.addOrdered(node);
end;

destructor TXQValueSequence.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;

{ TXQValueNode }

{$ImplicitExceptions off}

procedure TXQValueNode.setNode(AValue: TTreeNode);
begin
  if fnode=AValue then Exit;
  if fnode <> nil then fnode.getDocument().release;
  fnode:=AValue;
  if fnode <> nil then fnode.getDocument().addRef;
end;

constructor TXQValueNode.create();
begin
  inherited create(baseSchema.node);
end;

constructor TXQValueNode.create(anode: TTreeNode);
begin
  inherited create(baseSchema.node);
  fnode := anode;
  //assert((fnode <> nil) and (fnode.getDocument() <> nil));
  fnode.getDocument().addRef;
end;

destructor TXQValueNode.destroy;
begin
  if fnode <> nil then fnode.getDocument().release;
  inherited destroy;
end;

{$ImplicitExceptions on}

class function TXQValueNode.classKind: TXQValueKind;
begin
  Result:=pvkNode;
end;

function TXQValueNode.toBoolean: boolean;
begin
 Result:=node <> nil;
end;

function TXQValueNode.toBooleanEffective: boolean;
begin
  Result:=true;
end;





function TXQValueNode.toString: string;
begin
  result := TXQueryInternals.treeElementAsString(node);
end;

function TXQValueNode.toDateTime: TDateTime;
begin
  result := StrToDateTimeDef(toString,0);
end;

function TXQValueNode.toFloatChecked(scontext: TXQStaticContext): xqfloat;
var
  str: String;
begin
  ignore(scontext);
  if scontext.strictTypeChecking and (nodeTypeAnnotation(node) = baseSchema.string_) then raiseXPTY0004TypeError(self, 'float/double');
  str := toString;
  result := xqfloat.parse(str);
  if IsNan(result) and (str <> 'NaN') then raiseFORG0001InvalidConversion(self, 'float/double')
end;

function TXQValueNode.toNode: TTreeNode;
begin
  result := node;
end;

function TXQValueNode.hashCode: uint32;
var
  temp: String;
begin
  temp := toString;
  result := TXQDefaultTypeInfo.hash(pchar(temp), length(temp));
end;

function TXQValueNode.clone: IXQValue;
begin
  result := TXQValueNode.Create(node);
end;

procedure TXQValueNode.jsonSerialize(var serializer: TXQSerializer);
begin
  if node <> nil then begin
    case node.typ of
      tetAttribute: if serializer.standard then serializer.error('SENR0001', 'Attribute', self)
                    else case serializer.nodeFormat of
                      tnsText: serializer.appendJSONString(toString);
                      else serializer.appendJSONString(node.getNodeName()+'="'+(node as TTreeAttribute).realvalue+'"');
                    end
      else
        case serializer.nodeFormat of
          tnsText: serializer.appendJSONString(toString);
          tnsXML: serializer.appendJSONString(node.outerXML());
          tnsHTML: serializer.appendJSONString(node.outerHTML());
        end;
    end;
  end else serializer.append('null');
end;

procedure TXQValueNode.xmlSerialize(var serializer: TXQSerializer);
begin
  if node <> nil then begin
    case serializer.nodeFormat of
      tnsText: serializer.appendXMLText(toString);
      tnsXML: serializer.appendXMLText(node.outerXML()); //is this supposed to be like this?
      tnsHTML: serializer.appendXMLText(node.outerHTML());
    end;
  end
end;

procedure TXQValueNode.adaptiveSerialize(var serializer: TXQSerializer);
begin
  case node.typ of
    tetAttribute: begin
      if node.namespace <> nil then begin
        serializer.append(node.getNamespacePrefix());
        serializer.append(':');
      end;
      serializer.append(node.value);
      serializer.append('="');
      serializer.appendXMLAttrib((node as TTreeAttribute).realvalue);
      serializer.append('"');
    end;
    else serializer.append(node.outerXML());
  end;
end;

function TXQValueNode.stringifyNodes: IXQValue;
begin
  Result:=xqvalue(toString);
end;

function TXQValueNode.hasNodes: boolean;
begin
  result := true;
end;

class function TXQValueNode.nodeTypeAnnotation(tn: TTreeNode): TXSType;
begin
  case tn.typ of
    tetOpen, tetDocument: result := baseSchema.untyped; //todo: handle i:type="xs:integer" attribute
    tetText: result := baseSchema.untypedAtomic;
    tetAttribute: if (tn as TTreeAttribute).isNamespaceNode then result := baseSchema.anyURI else result := baseSchema.untypedAtomic;
    tetComment, tetProcessingInstruction: result := baseSchema.string_;
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'Impossible node type');
  end;
end;

class function TXQValueNode.nodeTypeAnnotationForAtomic(tn: TTreeNode): TXSType;
begin
  case tn.typ of
    tetOpen, tetDocument: result := baseSchema.untypedAtomic; //todo: handle i:type="xs:integer" attribute
    tetText: result := baseSchema.untypedAtomic;
    tetAttribute: if (tn as TTreeAttribute).isNamespaceNode then result := baseSchema.anyURI else result := baseSchema.untypedAtomic;
    tetComment, tetProcessingInstruction: result := baseSchema.string_;
    else raise EXQEvaluationException.Create('pxp:INTERNAL', 'Impossible node type');
  end;
end;







function TXQValueGenericPropertyEnumerator.GetEnumerator: TXQValueGenericPropertyEnumerator;
begin
  result := self;
end;


class function TXQValueMapLike.classKind: TXQValueKind;
begin
  Result:=pvkObject;
end;

function TXQValueMapLike.toBooleanEffective: boolean;
begin
  Result:=true;
  if not globalCurrentQueryInfo.AllowJSONiqOperations then
    raise EXQEvaluationException.create('FORG0006', 'Object cannot be used as boolean', nil, self);
end;

function TXQValueMapLike.toString: string;
begin
  Result:=inherited toString;
  if not globalCurrentQueryInfo.AllowJSONiqOperations then
    raise EXQEvaluationException.create('FOTY0014', 'Object cannot be used as string', nil, self);
end;

function TXQValueMapLike.toJoinedString(const sep: string): string;
begin
  if not globalCurrentQueryInfo.AllowJSONiqOperations then
    raise EXQEvaluationException.create('FOTY0013', 'Object cannot be used as string', nil, self); //0013 because it is implicitly calling fn:data
  Result:=inherited toJoinedString(sep);
end;

function TXQValueMapLike.setImmutable(const props: PString; len: SizeInt; const v: IXQValue): TXQValueMapLike;
var
  oldValue: TXQValue;
  newValue: IXQValue;
begin
  if len = 1 then
    exit(setImmutable(props^, v));

  if not hasProperty(props^, @oldValue) then
    raise EXQEvaluationException.Create('pxp:OBJECT', 'Property '+props^+' not found, when assigning to '+strJoin(props, len, '.'));
  if oldValue.kind in [pvkArray, pvkObject] then
    newValue := oldValue.setImmutable(props + 1, len - 1, v)
  else
   raise EXQEvaluationException.Create('pxp:OBJECT', 'Need object or array property '+props^+' to assign to '+strJoin(props, len, '.'));

  result := setImmutable(props^, newValue);
end;



class function TXQValueMapLike.newinstance: tobject;
var hackmethod: TMethod;
begin
  hackMethod.Data := self;
  hackMethod.Code := @TObject.newinstance;
  result := TNewinstanceFunc(hackMethod)();
end;

procedure TXQValueMapLike.FreeInstance;
begin
  Freemem(pointer(self));
end;







constructor TXQValueStringMap.create();
begin
  inherited create(baseJSONiqSchema.object_);
  mapdata.init;
end;

destructor TXQValueStringMap.Destroy;
begin
  mapdata.done;
  mapdata := default(TXQHashmapStrOwningXQValue);
  inherited Destroy;
end;


function TXQValueStringMap.Size: SizeInt;
begin
  Result:=mapdata.Size;
end;

function TXQValueStringMap.getPropertyKeyKind: TXQMapPropertyKeyKind;
begin
  Result:=xqmpkkStringKeys;
end;

function TXQValueStringMap.hasProperty(const name: string; value: PXQValue): boolean;
var
  p: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
begin
  p := mapdata.findKeyValuePair(name);
  result := p.isAssigned;
  if result and assigned(value) then value^ := p.value;
end;

function TXQValueStringMap.hasProperty(const name: ixqvalue; value: PXQValue): boolean;
begin
  if TXQValueOwnershipTracker.isStringLikeAfterAtomize(name.toValue) or globalCurrentQueryInfo.CastStringMapKeysToString then
    Result := hasProperty(name.toString, value)
   else
    result := false;
end;

function TXQValueStringMap.getProperty(const name: IXQValue): IXQValue;
var temp: TXQValue;
begin
  if hasProperty(name, @temp) then result := temp
  else result := xqvalue();
end;

function TXQValueStringMap.getProperty(const name: string): IXQValue;
var
  p: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
begin
  p := mapdata.findKeyValuePair(name);
  if p.isAssigned then result := p.value
  else result := xqvalue();
end;


constructor TXQValueStringMap.TXQValueStringPropertyEnumeratorForStringMap.create(map: TXQValueStringMap);
begin
  enumerator := map.mapdata.GetEnumerator;
end;

function TXQValueStringMap.TXQValueStringPropertyEnumeratorForStringMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then fcurrent := enumerator.current;
end;

constructor TXQValueStringMap.TXQValuePropertyValueEnumeratorForStringMap.create(map: TXQValueStringMap);
begin
  enumerator := map.mapdata.GetEnumerator;
end;

function TXQValueStringMap.TXQValuePropertyValueEnumeratorForStringMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then fcurrent := enumerator.current.value;
end;

constructor TXQValueStringMap.TXQValueStandardPropertyEnumeratorForStringMap.create(map: TXQValueStringMap);
begin
  enumerator := map.mapdata.GetEnumerator;
  fcurrent.entity := @tempEntity;
end;

function TXQValueStringMap.TXQValueStandardPropertyEnumeratorForStringMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then begin
    tempEntity.key := TXQValueString.create(enumerator.current.key);
    tempEntity.value := enumerator.current.value;
  end;
end;


function TXQValueStringMap.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  Result := TXQValueStandardPropertyEnumeratorForStringMap.create(self);
end;

function TXQValueStringMap.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  Result:=TXQValuePropertyValueEnumeratorForStringMap.create(self);
end;

function TXQValueStringMap.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  Result:=TXQValueStringPropertyEnumeratorForStringMap.create(self);
end;


procedure TXQValueStringMap.setMutable(const name: string; const v: IXQValue);
begin
  mapdata.include(name, v.toValue);
end;

function TXQValueStringMap.setImmutable(const name: string; const v: IXQValue): TXQValueMapLike;
const SMALL_SIZE_FULL_COPY = 7;
var
  Resmap: TXQValueStringMap;
  resmapupdate: TXQValueStringMapPendingUpdate;
begin
  if (Size <= SMALL_SIZE_FULL_COPY) or hasProperty(name, nil) then begin
    resmap := TXQValueStringMap.create;
    resmap.mapdata.assign(mapdata);
    resmap.setMutable(name, v);
    result := resmap
  end else begin
    resmapupdate := TXQValueStringMapPendingUpdate.create(self);
    resmapupdate.mapdata.include(name, v.toValue);
    result := resmapupdate
  end;
end;

function TXQValueStringMap.setImmutable(const name, value: IXQValue): TXQValueMapLike;
var
  sp: TXQStandardProperty;
  res: TXQValueStandardMap;
begin
  if (name.typeAnnotation = baseSchema.string_) or globalCurrentQueryInfo.CastStringMapKeysToString then
    exit(setImmutable(name.toString, value));
  res := TXQValueStandardMap.create();
  result := res;
  for sp in getEnumeratorPropertiesUnsafe do
    res.mapdata.include(sp.key, sp.value);
  res.setMutable(name, value);
end;

procedure TXQValueStringMap.setMutable(const name: string; const s: string);
begin
  setMutable(name, xqvalue(s));
end;



procedure TXQValueStringMap.enumeratePropertyKeys(var keyset: TXQHashsetStr);
var p: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
begin
  for p in mapdata do
    keyset.include(p.key);
end;

function TXQValueStringMap.enumeratePropertyValues: IXQValue;
var p: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
  tempseq: TXQValueSequence;
begin
  tempseq := TXQValueSequence.create(mapdata.Count);
  for p in mapdata do
    tempseq.add(p.value);
  result := tempseq;
end;


function TXQValueStringMap.clone: IXQValue;
var
  resmap: TXQValueStringMap;
begin
  resmap := TXQValueStringMap.create;
  resmap.mapdata.assign(mapdata);
  Result:=resmap
end;









constructor TXQValueStringMapPendingUpdate.create();
begin
  inherited;
end;

constructor TXQValueStringMapPendingUpdate.create(p: TXQValueStringMap);
begin
  inherited create;
  prototype := p
end;

destructor TXQValueStringMapPendingUpdate.Destroy;
begin
  prototype := nil;
  inherited Destroy;
end;

function TXQValueStringMapPendingUpdate.Size: SizeInt;
begin
  result := inherited size;
  if prototype <> nil then result += prototype.Size;
end;

function TXQValueStringMapPendingUpdate.hasProperty(const name: string; value: PXQValue): boolean;
begin
  Result:=inherited hasProperty(name, value);
  if not result and assigned(prototype) then result := prototype.hasProperty(name, value);
end;

function TXQValueStringMapPendingUpdate.getProperty(const name: string): IXQValue;
var
  temp: TXQValue;
begin
  if hasProperty(name, @temp) then result := temp
  else result := xqvalue()
end;

function TXQValueStringMapPendingUpdate.getEnumerators: TXQHashmapRawEnumerators;
begin
  if prototype <> nil then begin
    result[0] := (prototype.toValue as txqvaluestringmap).mapdata.getEnumerator;
    result[1] := mapdata.getEnumerator;
  end else begin
    result[0] := mapdata.getEnumerator;
    result[1] := default(TXQHashmapStrOwningXQValue.TKeyPairEnumerator);
  end;
end;

class function TXQValueStringMapPendingUpdate.moveNextEnumerators(var enums: TXQHashmapRawEnumerators): boolean;
begin
  result :=  enums[0].moveNext;
  if (not result) and (enums[1].map <> nil) then begin
    enums[0] := enums[1];
    enums[1].map := nil;
    result := enums[0].moveNext;
  end;
end;

constructor TXQValueStringMapPendingUpdate.TXQValueStringPropertyEnumeratorForStringMapPendingUpdate.create(
  map: TXQValueStringMapPendingUpdate);
begin
  enumerators := map.getEnumerators;
end;

function TXQValueStringMapPendingUpdate.TXQValueStringPropertyEnumeratorForStringMapPendingUpdate.MoveNext: Boolean;
begin
  result := moveNextEnumerators(enumerators);
  if result then
    fcurrent := enumerators[0].current;
end;

constructor TXQValueStringMapPendingUpdate.TXQValuePropertyValueEnumeratorForStringMapPendingUpdate.create(
  map: TXQValueStringMapPendingUpdate);
begin
  enumerators := map.getEnumerators;
end;

function TXQValueStringMapPendingUpdate.TXQValuePropertyValueEnumeratorForStringMapPendingUpdate.MoveNext: Boolean;
begin
  result := moveNextEnumerators(enumerators);
  if result then
    fcurrent := enumerators[0].current.value;
end;

constructor TXQValueStringMapPendingUpdate.TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate.create(
  map: TXQValueStringMapPendingUpdate);
begin
  enumerators := map.getEnumerators;
  fcurrent.entity := @tempEntity;
end;

function TXQValueStringMapPendingUpdate.TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate.MoveNext: Boolean;
begin
  result := moveNextEnumerators(enumerators);
  if result then begin
    tempEntity.key := TXQValueString.create(enumerators[0].current.key);
    tempEntity.value := enumerators[0].current.value;
  end;
end;


function TXQValueStringMapPendingUpdate.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  Result:=TXQValueStandardPropertyEnumeratorForStringMapPendingUpdate.create(self);
end;

function TXQValueStringMapPendingUpdate.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  Result:=TXQValuePropertyValueEnumeratorForStringMapPendingUpdate.create(self);
end;

function TXQValueStringMapPendingUpdate.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  Result:=TXQValueStringPropertyEnumeratorForStringMapPendingUpdate.create(self);
end;

procedure TXQValueStringMapPendingUpdate.setMutable(const name: string; const v: IXQValue);
  procedure internalClone;
  var temp: TXQHashmapStrOwningXQValue;
  begin
    temp.init;
    temp.assign((prototype.toValue as TXQValueStringMap).mapdata);
    temp.includeAll(mapdata);
    mapdata.assign(temp);
    temp.done;
    prototype := nil
  end;

begin
{var
  pair: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
begin
  pair := mapdata.findKeyValuePair(name);
  if pair.isAssigned then
    mapdata.include(name, v.toValue) //pair.value := v;
  else if (prototype  nil) or not (prototype.hasProperty(name, nil)) then include}
  if assigned(prototype) and prototype.hasProperty(name, nil) then
    internalClone;
  mapdata.include(name, v.toValue)
end;

procedure TXQValueStringMapPendingUpdate.setMutable(const name: string; const s: string);
begin
  setMutable(name, xqvalue(s))
end;

function TXQValueStringMapPendingUpdate.setImmutable(const name: string; const v: IXQValue): TXQValueMapLike;
const SMALL_UPDATE_COUNT = 7;
var
  resmap: TXQValueStringMap;
begin
  if prototype = nil then
    result := inherited
  else begin
    if (mapdata.Count <= SMALL_UPDATE_COUNT) and not prototype.hasProperty(name, nil) then begin
      resmap := TXQValueStringMapPendingUpdate.create(prototype.toValue as TXQValueStringMap);
      resmap.mapdata.assign(mapdata);
    end else begin
      resmap := txqvaluestringmap.create();
      resmap.mapdata.assign((prototype as txqvaluestringmap).mapdata);
      resmap.mapdata.includeAll(mapdata);
    end;
    resmap.mapdata.include(name, v.toValue);
    result := resmap
  end;
end;

procedure TXQValueStringMapPendingUpdate.enumeratePropertyKeys(var keyset: TXQHashsetStr);
begin
  if prototype <> nil then prototype.enumeratePropertyKeys(keyset);
  inherited enumeratePropertyKeys(keyset);
end;

function TXQValueStringMapPendingUpdate.enumeratePropertyValues: IXQValue;
var
  seq: TXQValueSequence = nil;
  p: TXQHashmapStrOwningXQValue.TKeyValuePairOption;
begin
  if prototype <> nil then begin
    result := prototype.enumeratePropertyValues;
    if result is TXQValueSequence then seq := result as TXQValueSequence;
  end else result := nil;
  for p in mapdata do
    xqvalueSeqConstruct(result, seq, p.value);
end;

function TXQValueStringMapPendingUpdate.clone: IXQValue;
var
  resmap: TXQValueStringMapPendingUpdate;
begin
  resmap := TXQValueStringMapPendingUpdate.create;
  resmap.prototype := prototype;
  resmap.mapdata.assign(mapdata);
  Result:=resmap
end;




constructor TXQValueStandardMap.create();
begin
  inherited create(baseJSONiqSchema.object_);
  mapdata.init();
end;

constructor TXQValueStandardMap.create(const othermapdata: TXQHashmapXQValue);
begin
  inherited create(baseJSONiqSchema.object_);
  mapdata.init;
  mapdata.assign(othermapdata);
end;

destructor TXQValueStandardMap.Destroy;
begin
  mapdata.done;
  inherited Destroy;
end;

function TXQValueStandardMap.Size: SizeInt;
begin
  Result:=mapdata.Count;
end;

function TXQValueStandardMap.getPropertyKeyKind: TXQMapPropertyKeyKind;
begin
  Result:=xqmpkkStandardKeys;
end;

function TXQValueStandardMap.findKeyValuePair(const key: IXQValue): TXQHashmapXQValue.TKeyValuePairOption;
  function getAtomized: TXQHashmapXQValue.TKeyValuePairOption;
  begin
    result := mapdata.findKeyValuePair(xqvalueAtomize(key))
  end;
begin
  if TXQValueOwnershipTracker.isAtomicKeyValue(key) then
    result := mapdata.findKeyValuePair(key)
   else
    result := getAtomized
end;

function TXQValueStandardMap.hasProperty(const name: string; value: PXQValue): boolean;
begin
  result := hasProperty(xqvalue(name), value);
end;


function TXQValueStandardMap.getProperty(const name: string): IXQValue;
begin
  result := getProperty(xqvalue(name));
end;

function TXQValueStandardMap.hasProperty(const key: IXQValue; value: PXQValue): boolean;
var
  p: TXQHashmapXQValue.TKeyValuePairOption;
begin
  p := findKeyValuePair(key);
  result := p.isAssigned;
  if result and assigned(value) then value^ := p.value;
end;

function TXQValueStandardMap.getProperty(const key: IXQValue): IXQValue;
var
  p: TXQHashmapXQValue.TKeyValuePairOption;
begin
  p := findKeyValuePair(key);
  if p.isAssigned then result := p.value
  else result := xqvalue();
end;

constructor TXQValueStandardMap.TXQValueStringPropertyEnumeratorForStandardMap.create(map: TXQValueStandardMap);
begin
  enumerator := map.mapdata.getEnumerator;
  fcurrent.entity := @tempEntity;
end;

function TXQValueStandardMap.TXQValueStringPropertyEnumeratorForStandardMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then begin
    tempEntity.Key := enumerator.current.key.toString;
    tempEntity.Value := enumerator.current.value;
  end;
end;

constructor TXQValueStandardMap.TXQValueStandardPropertyEnumeratorForStandardMap.create(map: TXQValueStandardMap);
begin
  enumerator := map.mapdata.getEnumerator;
end;

function TXQValueStandardMap.TXQValueStandardPropertyEnumeratorForStandardMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then
    fcurrent := enumerator.current;
end;

constructor TXQValueStandardMap.TXQValuePropertyValueEnumeratorForStandardMap.create(map: TXQValueStandardMap);
begin
  enumerator := map.mapdata.getEnumerator;
end;

function TXQValueStandardMap.TXQValuePropertyValueEnumeratorForStandardMap.MoveNext: Boolean;
begin
  result := enumerator.moveNext;
  if result then
    fcurrent := enumerator.current.value;
end;


function TXQValueStandardMap.getEnumeratorPropertiesUnsafe: TXQValueStandardPropertyEnumerator;
begin
  Result:=TXQValueStandardPropertyEnumeratorForStandardMap.create(self);
end;

function TXQValueStandardMap.getEnumeratorPropertyValuesUnsafe: TXQValuePropertyValueEnumerator;
begin
  Result:=TXQValuePropertyValueEnumeratorForStandardMap.Create(self)
end;

function TXQValueStandardMap.getEnumeratorStringPropertiesUnsafe: TXQValueStringPropertyEnumerator;
begin
  Result:=TXQValueStringPropertyEnumeratorForStandardMap.create(self);
end;


procedure TXQValueStandardMap.setMutable(const name: string; const v: IXQValue);
begin
  mapdata.include(xqvalue(name), v.toValue);
end;

procedure TXQValueStandardMap.setMutable(const name: string; const s: string);
begin
  mapdata.include(xqvalue(name), TXQValueString.create(s));
end;

procedure TXQValueStandardMap.setMutable(const key: IXQValue; const v: IXQValue);
  procedure setAtomized;
  var temp: IXQValue;
  begin
    temp := xqvalueAtomize(key);
    if temp.getSequenceCount <> 1 then raiseXPTY0004TypeError(key, 'map key');
    mapdata.include(temp, v.toValue);
  end;

begin
  if TXQValueOwnershipTracker.isAtomicKeyValue(key) then
    mapdata.include(key, v.toValue)
   else
    setAtomized;
end;

function TXQValueStandardMap.setImmutable(const name, value: IXQValue): TXQValueMapLike;
var
  resmap: TXQValueStandardMap;
begin
  resmap := TXQValueStandardMap.create(mapdata);
  resmap.setMutable(name, value);
  result := resmap;
end;

function TXQValueStandardMap.setImmutable(const name: string; const v: IXQValue): TXQValueMapLike;
begin
  result := setImmutable(xqvalue(name), v);
end;

function TXQValueStandardMap.enumeratePropertyKeys: IXQValue;
var p: TXQHashmapXQValue.TKeyValuePairOption;
  list: TXQVList;
begin
  list := TXQVList.create(size);
  for p in mapdata do
    list.add(p.key);
  xqvalueSeqSqueezed(result, list);
end;

procedure TXQValueStandardMap.enumeratePropertyKeys(var keyset: TXQHashsetStr);
var p: TXQHashmapXQValue.TKeyValuePairOption;
begin
  for p in mapdata do
    keyset.include(p.key.toString);
end;

function TXQValueStandardMap.enumeratePropertyValues: IXQValue;
var p: TXQHashmapXQValue.TKeyValuePairOption;
  list: TXQVList;
begin
  list := TXQVList.create(size);
  for p in mapdata do
    list.add(p.value);
  xqvalueSeqSqueezed(result, list);
end;

function TXQValueStandardMap.clone: IXQValue;
begin
  Result:=TXQValueStandardMap.create(mapdata);
end;




{ TXQValueJSONArray }

constructor TXQValueJSONArray.create(takeList: TXQVList);
begin
  inherited create(baseJSONiqSchema.array_);
  seq := takeList
end;

constructor TXQValueJSONArray.create(capacity: SizeInt);
begin
  inherited create(baseJSONiqSchema.array_);
  seq := TXQVList.create(capacity);
end;

class function TXQValueJSONArray.classKind: TXQValueKind;
begin
  Result:=pvkArray;
end;

function TXQValueJSONArray.isUndefined: boolean;
begin
  Result:=false;
end;

function TXQValueJSONArray.GetEnumeratorMembers: TXQValueEnumerator;
begin
  result.fguardian := self;
  result.ptr := GetEnumeratorMembersPtrUnsafe;
end;

function TXQValueJSONArray.GetEnumeratorMembersPtrUnsafe: TXQValueEnumeratorPtrUnsafe;
begin
  if seq.Count > 0 then begin
    result.fcurrent:=@seq.fbuffer[0];
    dec(result.fcurrent);
    result.flast := @seq.fbuffer[seq.Count-1];
  end else TXQValueEnumeratorPtrUnsafe.clear(result);
end;

function TXQValueJSONArray.Size: SizeInt;
begin
  result := seq.Count;
end;

function TXQValueJSONArray.toBooleanEffective: boolean;
begin
  result := true;
  if not globalCurrentQueryInfo.AllowJSONiqOperations then
    raise EXQEvaluationException.create('FORG0006', 'Array cannot be used as boolean', nil, self);
end;

function TXQValueJSONArray.toString: string;
begin
  result := toJoinedString('');
end;

function TXQValueJSONArray.toJoinedString(const sep: string): string;
var
  sb: TStrBuilder;
  i: sizeint;
begin
  case seq.Count of
    0: exit('');
    1: exit(seq[0].toJoinedString(sep));
  end;
  sb.init(@result);
  sb.append(seq[0].toJoinedString(sep));
  for i := 1 to seq.Count - 1 do begin
    sb.append(sep);
    sb.append(seq[i].toJoinedString(sep));
  end;
  sb.final;
end;

function TXQValueJSONArray.clone: IXQValue;
var
  i: SizeInt;
  seqr: TXQValueJSONArray;
begin
  seqr := TXQValueJSONArray.Create(seq.Count);
  for i:=0 to seq.Count-1 do
    seqr.seq.Add(seq[i].clone);
  result := seqr;
end;

function TXQValueJSONArray.setImmutable(const props: PString; len: SizeInt; const v: IXQValue): TXQValue;
var
  newValue: IXQValue;
  i: SizeInt;
  idx: SizeInt;
  resultArray: TXQValueJSONArray;
  resseq: TXQVList;
begin
  idx := StrToInt64(props^);
  if idx < 0 then raise EXQEvaluationException.create('pxp:ARRAY', 'Cannot assign to negative array index');

  if (len = 1) then
    newValue := v
  else if (idx > seq.Count) then
    raise EXQEvaluationException.Create('pxp:Array', 'Element index'+props^+' not found, when assigning to '+strJoin(props, len, '.'))
  else if seq[idx-1].kind in [pvkArray, pvkObject] then
    newValue := seq[idx-1].toValue.setImmutable(props + 1, len - 1, v)
  else raise EXQEvaluationException.Create('pxp:ARRAY', 'Need array or object '+props^+' to assign to '+strJoin(props, len, '.'));

  resultArray := TXQValueJSONArray.create(seq.Count);
  resseq := resultArray.seq;
  if idx = 0 then resseq.add(newValue);
  idx -= 1;
  for i := 0 to seq.Count - 1 do
    if i <> idx  then resseq.add(seq[i])
    else resseq.add(newValue);
  if idx >= seq.Count then resseq.add(newValue); //e.g. $a(1000) := ..
  result := resultArray;
end;

procedure TXQValueJSONArray.jsonSerialize(var serializer: TXQSerializer);
var
  i: SizeInt;
  ws: TXQSerializerInsertWhitespace;
begin
  case seq.Count of
    0: serializer.appendJSONEmptyArray;
    else begin
      ws := serializer.insertWhitespace;
      if (ws = xqsiwIndent) and (seq.Count <= 8) then begin
        serializer.insertWhitespace := xqsiwConservative;
        for i := 0 to seq.Count - 1 do
          if seq[i].kind in [pvkObject,pvkArray] then begin
            serializer.insertWhitespace := xqsiwIndent;
            break;
          end;
      end;
      with serializer do begin
        appendJSONArrayStart;
        seq[0].jsonSerialize(serializer);
        for i := 1 to seq.Count-1 do begin
          appendJSONArrayComma();
          seq[i].jsonSerialize(serializer);
        end;
        appendJSONArrayEnd;
      end;
      serializer.insertWhitespace := ws;
    end;
  end;
end;

procedure TXQValueJSONArray.xmlSerialize(var serializer: TXQSerializer);
var
  i: SizeInt;
begin
  with serializer do
    if seq.Count = 0 then
      appendXMLEmptyElement(sequenceTag)
     else begin
       appendXMLElementStartTag(sequenceTag);
       appendXMLElementStartTag(elementTag);
       seq[0].xmlSerialize(serializer);
       appendXMLElementEndTag(elementTag);
       for i := 1 to seq.Count-1 do begin
         appendXMLElementStartTag(elementTag);
         seq[i].xmlSerialize(serializer);
         appendXMLElementEndTag(elementTag);
       end;
       appendXMLElementEndTag(sequenceTag);
     end;
end;

procedure TXQValueJSONArray.adaptiveSerialize(var serializer: TXQSerializer);
var
  i: SizeInt;
begin
  serializer.appendJSONArrayStart;
  if seq.Count > 0 then begin
    seq[0].adaptiveSerialize(serializer);
    for i := 1 to seq.Count-1 do begin
      serializer.appendJSONArrayComma();
      seq[i].adaptiveSerialize(serializer);
    end;
  end;
  serializer.appendJSONArrayEnd;
end;

function TXQValueJSONArray.stringifyNodes: IXQValue;
begin
  Result:= TXQValueJSONArray.create(seq.stringifyNodes);
end;

function TXQValueJSONArray.hasNodes: boolean;
begin
  Result:= seq.hasNodes;
end;


destructor TXQValueJSONArray.Destroy;
begin
  seq.Free;
  inherited Destroy;
end;


constructor TXQValueJSONNull.create;
begin
  inherited Create(baseJSONiqSchema.jsNull)
end;

class function TXQValueJSONNull.classKind: TXQValueKind;
begin
  Result:=pvkNull;
end;

function TXQValueJSONNull.clone: IXQValue;
begin
  Result:=TXQValueJSONNull.create();
end;

function TXQValueJSONNull.toString: string;
begin
  result:='null';
end;

function TXQValueJSONNull.hashCode: uint32;
begin
  result := 0;
end;

procedure TXQValueJSONNull.jsonSerialize(var serializer: TXQSerializer);
begin
  serializer.append('null');
end;

procedure TXQValueJSONNull.xmlSerialize(var serializer: TXQSerializer);
begin
  serializer.appendXMLEmptyElement(serializer.sequenceTag);
end;



{ TXQValueFunction }

constructor TXQValueFunction.create(aterm: TXQTerm);
begin
  body := aterm;
  ftypeAnnotation := baseSchema.function_;
end;

procedure TXQValueFunction.FreeInstance;
var hackMethod: TMethod;
begin
  hackMethod.Code := @TObject.FreeInstance;
  hackMethod.Data := self;
  TProcedureOfObject(hackMethod)();
end;

destructor TXQValueFunction.Destroy;
var
  i: SizeInt;
begin
  if ownsTerms then begin
    body.free;
    for i := 0 to high(parameters) do begin
      parameters[i].seqtype.Free;
      parameters[i].variable.Free;
    end;
    freeAnnotations(annotations);
    resulttype.free;
  end;
  inherited Destroy;
end;

class function TXQValueFunction.classKind: TXQValueKind;
begin
  Result:=pvkFunction;
end;

function TXQValueFunction.toBooleanEffective: boolean;
begin
  raise EXQEvaluationException.Create('FORG0006', 'A function cannot be used a boolean');
  result := false;
end;


function TXQValueFunction.evaluate(const outerContext: TXQEvaluationContext; const term: TXQTerm): IXQValue;
var
  tempcontext: TXQEvaluationContext;
  j: SizeInt;
  evilkids: PIXQValue;
begin
  tempcontext := context;
  tempcontext.temporaryVariables := outerContext.temporaryVariables;
  tempcontext.globallyDeclaredVariables := outerContext.globallyDeclaredVariables;
  if length(parameters) > 0 then begin
    evilkids := outercontext.temporaryVariables.topptr(high(parameters));
    for j := 0 to high(parameters) do
      TXQAbstractFunctionInfo.convertType(evilkids[j], parameters[j].seqtype, context, term);
  end;
  result := body.evaluate(tempcontext);
  TXQAbstractFunctionInfo.convertType(result, resulttype, context, term);
end;

function TXQValueFunction.evaluateInContext(var inContext: TXQEvaluationContext; const term: TXQTerm): IXQValue;
var
  j: SizeInt;
  evilkids: PIXQValue;
begin
  if length(parameters) > 0 then begin
    evilkids := inContext.temporaryVariables.topptr(high(parameters));
    for j := 0 to high(parameters) do
      TXQAbstractFunctionInfo.convertType(evilkids[j], parameters[j].seqtype, inContext, term);
  end;
  result := body.evaluate(inContext);
  TXQAbstractFunctionInfo.convertType(result, resulttype, inContext, term);
end;

procedure TXQValueFunction.contextOverrideParameterNames(const inContext: TXQEvaluationContext; count: SizeInt);
{$IFDEF TRACK_STACK_VARIABLE_NAMES}
var
  stack: TXQEvaluationStack;
  i: SizeInt;
{$endif}
begin
  if count <> length(parameters) then
    raise EXQEvaluationException.create('err:XPTY0004', 'Mismatched argument count for function '+name+': got '+inttostr(count) + ' <> expected ' + inttostr(length(parameters)));
  {$IFDEF TRACK_STACK_VARIABLE_NAMES}
  if length(parameters) = 0 then exit;
  stack := inContext.temporaryVariables;
  if length(stack.debugNames) < stack.fcapacity then SetLength(stack.debugNames, stack.fcapacity);
  for i := 0 to high(parameters) do
    stack.debugNames[stack.fcount - high(parameters) - 1 + i] := parameters[i].variable.value;
  {$endif}
end;

function TXQValueFunction.directClone: TXQValue;
var
  f: TXQValueFunction;
begin
  f := TXQValueFunction.create(body);
  f.name := name;
  f.namespaceURL := namespaceURL;
  f.namespacePrefix := namespacePrefix;
  f.annotations := annotations;
  f.parameters := parameters;
  SetLength(f.parameters, length(f.parameters));
  f.resulttype := resulttype;
  f.context := context;
  if ownsTerms then f.assignCopiedTerms(self);
  result := f;
end;

function TXQValueFunction.clone: IXQValue;
begin
  result := directClone;
end;

function TXQValueFunction.toXQuery: string;
var
  i: SizeInt;
begin
  result := 'function (';
  for i := 0 to high(parameters) do begin
    if i <> 0 then result += ', ';
    result += parameters[i].toString;
  end;
  result += ')';
  if resulttype <> nil then result += ' as ' + resulttype.serialize;
  if body <> nil then result += '{' + body.debugTermToString + '}';
end;

function TXQValueFunction.debugAsStringWithTypeAnnotation(textOnly: boolean): string;
var
  i: SizeInt;
begin
  ignore(textOnly);
  result := 'function (';
  for i := 0 to high(parameters) do begin
    if i <> 0 then result += ', ';
    result += parameters[i].toString();
  end;
  result += ')';
  if resulttype <> nil then result += ' as ' + resulttype.debugTermToString;
end;


procedure TXQValueFunction.assignCopiedTerms(const func: TXQValueFunction);
var
  i: SizeInt;
begin
  body := func.body.clone;
  SetLength(parameters, length(func.parameters));
  for i := 0 to high(func.parameters) do begin
    if func.parameters[i].seqtype <> nil then parameters[i].seqtype := func.parameters[i].seqtype.clone as TXQTermSequenceType;
    if func.parameters[i].variable <> nil then parameters[i].variable := func.parameters[i].variable.clone as TXQTermVariable;
  end;
  if func.resulttype <> nil then
    resulttype := func.resulttype.clone as TXQTermSequenceType;
  copyAnnotations(annotations, func.annotations);
  ownsTerms := true;
end;

procedure TXQValueFunction.adaptiveSerialize(var serializer: TXQSerializer);
begin
  if (namespaceURL = '') and (name = '') then serializer.append('(anonymous-function)')
  else begin
    case namespaceURL of
      'http://www.w3.org/2005/xpath-functions': serializer.append('fn:');
      'http://www.w3.org/2005/xpath-functions/math': serializer.append('math:');
      'http://www.w3.org/2005/xpath-functions/map': serializer.append('map:');
      'http://www.w3.org/2005/xpath-functions/array': serializer.append('array:');
      'http://www.w3.org/2001/XMLSchema': serializer.append('xs:');
      else begin
        serializer.append('Q{');
        serializer.append(namespaceURL);
        serializer.append('}');
      end;
    end;
    serializer.append(name);
  end;
  serializer.append('#');
  serializer.appendNumber(length(parameters));
end;

procedure TXQValueFunction.visit(visitor: TXQTerm_Visitor);
var
  i: SizeInt;
begin
  for i := 0 to high(parameters) do
    visitor.declare(@parameters[i].variable, nil);
  visitor.simpleTermVisit(@body, nil);
  for i := high(parameters) downto 0 do
    visitor.undeclare(@parameters[i].variable, nil);
end;

