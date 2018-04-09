{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

{$TYPEINFO ON}

unit vte_rttigrid;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, TypInfo, ComCtrls, vte_treedata, VirtualTrees;

type

  TVirtualRttiGrid = class(TVirtualStringTreeData)
  private
    fObj  : TPersistent;
  protected
    function  _DumpMethod(typd: PTypeData; const PropName: shortstring): string;
    function  _DumpProp(typ: TTypeKind): string;
    procedure _DumpObj(obj : TObject;const PropName : shortstring='';node : TVirtualListViewItem=nil);
    function  _DumpPropValue(Instance: TObject; const PropName: shortstring): variant;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetObject(const Value: TPersistent);
    function  GetObject: TPersistent;
  published
    { Published declarations }
  //property Obj: TPersistent read GetObject write SetObject;
  //property Items stored False;
  //property Columns stored False;
  //property Rtti : TRTTIEnabler read fRtti;
  end;
                                                    

implementation

{ TVirtualRttiGrid }

constructor TVirtualRttiGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fObj:=nil;

  Header.Columns.Clear;
  Header.Columns.Add.Text:='Name';
  Header.Columns.Add.Text:='Value';
  Header.Columns.Add.Text:='Type';

  Header.Options:=Header.Options+[hoVisible];

  Header.Columns[0].Width:=100;
  Header.Columns[1].Width:=50;
  Header.Columns[2].Width:=50;
end;

procedure TVirtualRttiGrid.SetObject(const Value: TPersistent);
begin
  if not(csDesigning in ComponentState) 
    then
      begin
        fObj:=Value;

        Data.BeginUpdate;
        Data.Clear;
        _DumpObj(fObj);
        Data.EndUpdate;
      end
end;

function TVirtualRttiGrid.GetObject: TPersistent;
begin
  Result:=fObj;
end;

function TVirtualRttiGrid._DumpMethod(typd : PTypeData;const PropName : shortstring) : string;
var
  n,
  i,sl,x : Integer;
  s      : String;
  ss     : ShortString;
  j      : TParamFlag;
begin
  case typd^.MethodKind of
     mkProcedure        : Result:='Procedure';
     mkFunction         : Result:='Function';
     mkConstructor      : Result:='Constructor';
     mkDestructor       : Result:='Destructor';
     mkClassProcedure   : Result:='ClassProcedure';
     mkClassFunction    : Result:='ClassFunction';
     mkClassConstructor : Result:='ClassConstructor';
     mkClassDestructor  : Result:='ClassDestructor';
     mkOperatorOverload : Result:='OperatorOverload';
  end;

  n:=0;
  s:=' '+PropName+'(';
  for i:=0 to typd^.ParamCount-1 do
    begin
      {Just a collection of packed...
        record
          Flags: TParamFlags;
          ParamName: ShortString;
          TypeName: ShortString;
        end;
      }
      //Retrieve ParamFlag(s)
      for j:=pfVar to pfOut do
        if (j in TParamFlags(pword(@typd^.ParamList[n])^))  //=== ct9999 FPC SVN 35261============
          then
            case j of
              pfVar        : s:=s+'Var ';
              pfConst      : s:=s+'Const ';
              pfArray      : s:=s+'Array ';
              pfAddress    : s:=s+'Address ';
              pfReference  : s:=s+'Reference ';
              pfOut        : s:=s+'Out ';
            end;
      Inc(n);
      //Retrieve ParamName
      ss:='';
      sl:=Byte(typd^.ParamList[n]);
      for x:=n to n+sl do
        ss[x-n]:=typd^.ParamList[x];
      s:=s+ss;
      Inc(n,sl+1);

      //Retrieve ParamType
      sl:=Byte(typd^.ParamList[n]);
      ss:='';
      for x:=n to n+sl do
        ss[x-n]:=typd^.ParamList[x];
      Inc(n,sl+1);
      s:=s+': '+ss;
      if (i<>typd^.ParamCount-1)
        then s:=s+';'
    end;
  s:=s+')';

  //if typd^.MethodKind in [mkFunction,mkClassFunction,mkClassProcedure,mkClassConstructor,
  //                         mkClassDestructor]

  if typd^.MethodKind in [mkProcedure,mkFunction,mkConstructor,mkDestructor,
                          mkClassProcedure,mkClassFunction,mkClassConstructor,
                          mkClassDestructor]
    then
      begin
      //Retrieve Return Type
        s:=s+' : ';
        sl:=Byte(typd^.ParamList[n]);
        for x:=n to n+sl do
          ss[x-n]:=typd^.ParamList[x];
        s:=s+ss;
      //Inc(n,sl+1);
      end;

  if typd^.MethodKind in [mkOperatorOverload]
    then
      begin
        s:=s+'; overload';
      end;

  Result:=Result+s+';';
end;

function TVirtualRttiGrid._DumpProp(typ : TTypeKind) : String;
begin
  Result:='';
  case typ of
    tkUnknown     : Result:='Unknown';
    tkInteger     : Result:='Integer';
    tkChar        : Result:='Char';
    tkEnumeration : Result:='Enum';
    tkFloat       : Result:='Float';
    tkString      : Result:='String';
    tkSet         : Result:='Set';
    tkClass       : Result:='Class';
    tkMethod      : ;
    tkWChar       : Result:='WChar';
    tkLString     : Result:='LString';
    tkWString     : Result:='WString';
    tkVariant     : Result:='Variant';
    tkArray       : Result:='Array';
    tkRecord      : Result:='Record';
    tkInterface   : Result:='Interface';
    tkInt64       : Result:='Int64';
    tkDynArray    : Result:='DynArray';
  end;
end;

function TVirtualRttiGrid._DumpPropValue(Instance: TObject; const PropName: ShortString) : Variant;
var
  Method   : TMethod;
  PropInfo : PPropInfo;
begin
  Result:=GetPropValue(Instance,PropName,True);
  if (Result<>Null)
    then
      case PropType(Instance,PropName) of
        tkSet    : Result:='['+Result+']';
        tkClass  : Result:='0x'+IntToHex(Dword(Result),8);
        tkMethod : begin
                     PropInfo:=GetPropInfo(Instance,PropName);
                     Method:=GetMethodProp(Instance,PropInfo);
                     if (Method.Code=nil) and
                        (Method.Data=nil)
                       then Result:='nil'
                   end;
      end
    else Result:='Null';
end;

procedure TVirtualRttiGrid._DumpObj(obj : TObject;const PropName : ShortString='';node : TVirtualListViewItem=nil);
var
 cnt,
 i        : Integer;
 TempList : PPropList;
 s        : String;
 aNode,
 aItem    : TVirtualListViewItem;
 aClass   : TClass;
 aObj     : TObject;

 PropInfo : PPropInfo;
 TypeData : PTypeData;
begin
  if not Assigned(Obj) then exit;
  
  cnt:=GetPropList(Obj.ClassInfo,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray],nil);

  GetMem(TempList, cnt * SizeOf(Pointer));

  GetPropList(Obj.ClassInfo, [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray], TempList);

  aNode:=node;
  s:=PropName;
  if (node=nil)
    then
      begin
        for i:=0 to cnt-1 do
          begin
            if CompareText(TempList^[i]^.Name,'Name')=0
              then
                begin
                  s:=GetStrProp(Obj,'Name');
                  Break;
                end;
          end;
        if (s<>'')
          then
            begin
              aNode:=Items.AddChild(node,s);
              aNode.SubItems.Add('-');
              aNode.SubItems.Add(Obj.ClassName);
            //aNode.ImageIndex:=-1;
              Expanded[aNode.Node]:=True;
            end;
      end;

  for i:=0 to cnt-1 do
    begin
      case PropType(Obj,TempList^[i]^.Name) of
          tkClass : begin
                      aClass:=GetObjectPropClass(Obj,TempList^[i]^.Name);
                      aObj  :=GetObjectProp(Obj,TempList^[i],aclass);
                      if Assigned(aObj)
                        then
                          begin
                            aItem :=Items.AddChild(aNode,TempList^[i]^.Name);
                            aItem.SubItems.Add(_DumpPropValue(Obj,TempList^[i]^.Name));
                            aItem.SubItems.Add(_DumpProp(PropType(Obj,TempList^[i]^.Name)));
                          //aItem.ImageIndex:=1;
                            if (aObj<>Self)
                              then _DumpObj(aObj,'',aItem);
                          end
                        else
                          begin
                            aItem :=Items.AddChild(aNode,TempList^[i]^.Name);
                            aItem.SubItems.Add('nil');
                            aItem.SubItems.Add(_DumpProp(PropType(Obj,TempList^[i]^.Name)));
                          //aItem.ImageIndex:=2;
                          end;
                     end;
                     
        tkMethod   : begin
                       // get the prop info
                       PropInfo:=GetPropInfo(Obj,TempList^[i]^.Name);
                       if PropInfo <> nil
                         then
                           begin
                             TypeData := GetTypeData(PropInfo^.PropType);
                             aItem:=Items.AddChild(aNode,TempList^[i]^.Name);
                             aItem.SubItems.Add(_DumpPropValue(Obj,TempList^[i]^.Name));
                             aItem.SubItems.Add(GetPropValue(Obj,TempList^[i]^.Name,True)+' = '+_DumpMethod(TypeData,TempList^[i]^.Name));
                           //aItem.ImageIndex:=3;
                           end;
                     end;
      else
        begin
          aItem:=Items.AddChild(aNode,TempList^[i]^.Name);
          aItem.SubItems.Add(_DumpPropValue(Obj,TempList^[i]^.Name));
          aItem.SubItems.Add(_DumpProp(PropType(Obj,TempList^[i]^.Name)));
        end;
      end;
    end;
  FreeMem(TempList);

  if Items.Count>0
    then Items[0].Expanded:=True;
end;

end.

