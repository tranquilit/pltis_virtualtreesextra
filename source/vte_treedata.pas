{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit vte_treedata;

{$mode delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, Imglist;

{.$DEFINE DEBUG}

type

  TVirtualListViewItem   = class;
  TVirtualListViewItems  = class;

  TVTDCompareEvent = procedure(Sender: TBaseVirtualTree; Node1, Node2: TVirtualListViewItem; Column: TColumnIndex;
                               var Result: Integer) of object;

  TVirtualStringTreeData = class(TCustomVirtualStringTree)
  private
    FVirtualListViewItems : TVirtualListViewItems;
    FOnCompareEvent: TVTDCompareEvent;
  protected
    function  GetOptionsClass: TTreeOptionsClass; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String); override;
    function  DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
                  var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    function  DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property Align;          
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BorderStyle;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DefaultText;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions;
    property Visible;
    property WantTabs;

  //Default EventHandlers
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    { This property is called when toAutoSort is enabled in the AutoOptions.}
    property OnCompareNodes : TVTDCompareEvent read FOnCompareEvent write FOnCompareEvent;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStructureChange;
    property OnUpdating;

  //New Properties
    {This will set the checktype of all nodes and in/exclude
     voCheckSupport from the VirtualTreeView's options. If set for a tree,
     all nodes will have checkboxes. If thats not the intention one can set
     (and quickly reset if nessecary) the checked property of the items that
     need a checkbox.}
//  property CheckBoxes : Boolean read FCheckBoxes write SetCheckBoxes;
    property Data : TVirtualListViewItems read FVirtualListViewItems write FVirtualListViewItems stored True;
    property Items: TVirtualListViewItems read FVirtualListViewItems write FVirtualListViewItems stored True;
    property Nodes: TVirtualListViewItems read FVirtualListViewItems write FVirtualListViewItems stored True;
  end;


  {A collection Item containing one line's data.}
  TVirtualListViewItem = class(TCollectionItem)
  private
    FSubItems: TStrings;
    FCaption: String;
    FNode: PVirtualNode;
    FImageIndex : TImageIndex;
    function GetChecked: Boolean;
    function GetLevel: Integer;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetSelected: Boolean;
    function GetFocused: Boolean;
    function GetExpanded: Boolean;
    function GetParent: TVirtualListViewItem;
    function GetImageIndex: TImageIndex;
    procedure SetChecked(const Value: Boolean);
    procedure SetSubItems(const Value: TStrings);
    procedure SetSelected(const Value: Boolean);
    procedure SetFocussed(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetNode(const Value: PVirtualNode);
    procedure SetCaption(const Value: String);
    procedure SetImageIndex(const Value : TImageIndex);
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property  Node : PVirtualNode read fNode write SetNode;
  published
    property Caption  : String   read  FCaption  write SetCaption;
    property Checked  : Boolean  read  GetChecked write SetChecked;
    property SubItems : TStrings read  FSubItems write SetSubItems;
    property Text     : String    read  FCaption  write SetCaption;
    property Count    : Integer   read GetCount;
    property Expanded : Boolean   read  GetExpanded   write SetExpanded;
    property HasChildren: Boolean read GetHasChildren;
    property Level    : Integer   read GetLevel;
    property Parent   : TVirtualListViewItem read GetParent;
    property Focused  : Boolean read GetFocused write SetFocussed;
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex;
    property Selected : Boolean read GetSelected  write SetSelected;
  end;

  {A collection containing the Tree/Listviews data.}
  TVirtualListViewItems = class(TOwnedCollection) //Owned
  private
    FVirtualTree : TVirtualStringTreeData;
    function  GetItem(Index: Integer): TVirtualListViewItem;
    procedure SetItem(Index: Integer; Value: TVirtualListViewItem);
  protected
    function  _Locate(const aNode: PVirtualNode): Integer;
    procedure _CheckBox(aNode : PVirtualNode);
  public
    function  Add: TVirtualListViewItem;
    function  Insert(Index: Integer): TVirtualListViewItem; overload;
    procedure Delete(Index: Integer);
    function  AddNode(Node: TVirtualListViewItem; const S: string): TVirtualListViewItem;
    function  AddChild(Node: TVirtualListViewItem; const S: string) : TVirtualListViewItem;
    function  AddChildFirst(Node: TVirtualListViewItem; const S: string) : TVirtualListViewItem;
    function  AddFirst(Node: TVirtualListViewItem; const S: string) : TVirtualListViewItem;
    function  Insert(Node: TVirtualListViewItem; const S: string): TVirtualListViewItem; overload;
    procedure DeleteNode(Node: TVirtualListViewItem);
    function  GetFirstNode: TVirtualListViewItem;
    procedure Clear;
    property  Items[Index: Integer]: TVirtualListViewItem read GetItem write SetItem; default;
  published
    property  VirtualTree : TVirtualStringTreeData read FVirtualTree write FVirtualTree;
  end;

implementation

{$IFNDEF DEBUG}
procedure OutputDebugString(Msg : PChar);
begin
end;
{$ENDIF}


{ TVirtualListViewItem }

procedure TVirtualListViewItem.Assign(Source: TPersistent);
begin
//inherited;

  FSubItems.Assign(TVirtualListViewItem(Source).SubItems);
  FCaption:=TVirtualListViewItem(Source).Caption;
  FImageIndex:=TVirtualListViewItem(Source).ImageIndex;
  FNode:=TVirtualListViewItem(Source).Node;

  TVirtualListViewItems(Collection).VirtualTree.Paint;
end;

constructor TVirtualListViewItem.Create(aCollection: TCollection);
begin
  inherited;

  FNode    :=nil;
  FCaption :='';
  FSubItems:=TStringList.Create;
end;

destructor TVirtualListViewItem.Destroy;
begin
  SetLength(FCaption,0);

  FreeAndNil(fSubItems);

  inherited;
end;

function TVirtualListViewItem.GetChecked: Boolean;
begin
  if Assigned(Node)
    then Result:=(Node^.CheckState=csCheckedNormal)
    else Result:=False; //Error
end;


function TVirtualListViewItem.GetCount: Integer;
begin
  if Assigned(Node)
    then Result:=TVirtualListViewItems(Collection).VirtualTree.ChildCount[Node]
    else Result:=-1; //Error
end;


function TVirtualListViewItem.GetExpanded: Boolean;
begin
  if Assigned(Node)
    then Result:=(TVirtualListViewItems(Collection).VirtualTree.Expanded[Node])
    else Result:=False; //Error
end;


function TVirtualListViewItem.GetFocused: Boolean;
begin
  if Assigned(Node)
    then Result:=(TVirtualListViewItems(Collection).VirtualTree.FocusedNode=Node)
    else Result:=False; //Error
end;


function TVirtualListViewItem.GetHasChildren: Boolean;
begin
  if Assigned(Node)
    then Result:=TVirtualListViewItems(Collection).VirtualTree.HasChildren[Node]
    else Result:=False; //Error
end;


function TVirtualListViewItem.GetImageIndex: TImageIndex;
begin
  Result:=FImageIndex;
end;

function TVirtualListViewItem.GetLevel: Integer;
begin
  if Assigned(Node)
    then Result:=TVirtualListViewItems(Collection).VirtualTree.GetNodeLevel(Node)
    else Result:=-1; //Error
end;


function TVirtualListViewItem.GetParent: TVirtualListViewItem;
begin
  if Assigned(Node) and (TVirtualListViewItems(Collection).VirtualTree.GetNodeLevel(Node)>0)
    then Result:=TVirtualListViewItems(Collection).Items[TVirtualListViewItems(Collection)._Locate(Node^.Parent)]
    else Result:=nil
end;


function TVirtualListViewItem.GetSelected: Boolean;
begin
  if Assigned(Node)
    then Result:=TVirtualListViewItems(Collection).VirtualTree.Selected[Node]
    else Result:=False; //Error
end;


procedure TVirtualListViewItem.SetCaption(const Value: String);
begin
  if Value<>FCaption
    then
      begin
        OutputDebugString(PChar(Format('Changing Caption %s to %s Index %d, Id %d',[FCaption,Value,Index,Id])));
        FCaption := Value;
        if Assigned(Node)
          then TVirtualListViewItems(Collection).VirtualTree.InvalidateNode(Node);

       with TVirtualStringTreeData(TVirtualListViewItems(Collection).VirtualTree) do
         if (Header.SortColumn<=0) and (toAutoSort in TVirtualTreeOptions(TreeOptions).AutoOptions) then
           SortTree(Header.SortColumn, Header.SortDirection, True);
      end;
end;


procedure TVirtualListViewItem.SetChecked(const Value: Boolean);
begin
  if Assigned(Node)
    then
      with TVirtualListViewItems(Collection).VirtualTree do
        begin
          case Value of
            True  : CheckState[Node]:=csCheckedNormal;
            False : CheckState[Node]:=csUnCheckedNormal;
          end;
          Node^.CheckType:=ctTriStateCheckBox;
          TStringTreeOptions(TreeOptions).MiscOptions:=TStringTreeOptions(TreeOptions).MiscOptions+[toCheckSupport];
        end
    else ;//Error
end;


procedure TVirtualListViewItem.SetExpanded(const Value: Boolean);
begin
  if Assigned(Node)
    then TVirtualListViewItems(Collection).VirtualTree.Expanded[Node]:=Value
    else ; //Error
end;


procedure TVirtualListViewItem.SetFocussed(const Value: Boolean);
begin
  if Assigned(Node)
    then TVirtualListViewItems(Collection).VirtualTree.FocusedNode:=Node
    else ; //Error
end;


procedure TVirtualListViewItem.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex:=Value;
end;

procedure TVirtualListViewItem.SetNode(const Value: PVirtualNode);
begin
  FNode:=Value;
end;


procedure TVirtualListViewItem.SetSelected(const Value: Boolean);
begin
  if Assigned(Node)
    then TVirtualListViewItems(Collection).VirtualTree.Selected[Node]:=Value
    else ; //Error
end;


procedure TVirtualListViewItem.SetSubItems(const Value: TStrings);
begin
  if Value <> nil
    then FSubItems.Assign(Value);
//veg:todo better..
{
  with TVirtualStringTreeData(TVirtualListViewItems(Collection).VirtualTree) do
    if (Header.SortColumn>0) and (toAutoSort in TVirtualTreeOptions(TreeOptions).AutoOptions) then
      SortTree(Header.SortColumn, Header.SortDirection, True);
}
end;


// =================== TVirtualListViewItems ============================

function TVirtualListViewItems.Add: TVirtualListViewItem;
begin
  Result:=TVirtualListViewItem(inherited Add);
  Result.Caption:='';
  Result.ImageIndex:=-1;
  Result.Node:=VirtualTree.AddChild(VirtualTree.RootNode,Pointer(Result.Id));
  _CheckBox(Result.Node);
  OutputDebugString(PChar(Format('Adding %s Index %d, Id %d',[Result.Caption,Result.Index,Result.Id])));  
end;


procedure TVirtualListViewItems.Delete(Index: Integer);
var
  i : Integer;
  aChild,
  aNode : PVirtualNode;
begin
  BeginUpdate;

  OutputDebugString(PChar(Format('Deleting %s Index %d, Id %d',[Items[Index].Caption,Index,Items[Index].Id])));

  aNode:=Items[Index].Node;
  for i:=aNode^.ChildCount downto 1 do
    begin
      aChild:=VirtualTree.GetFirstChild(aNode);
      Delete(_Locate(aChild)); //Recurse
    end;
  VirtualTree.DeleteNode(aNode);

  inherited;

  EndUpdate;
end;


function TVirtualListViewItems.Insert(
  Index: Integer): TVirtualListViewItem;
var
  aNode: PVirtualNode;
begin
  BeginUpdate;

  aNode:=Items[Index].Node;

  Result:=TVirtualListViewItem(inherited Insert(Index));

  Result.Node:=VirtualTree.InsertNode(aNode,amInsertAfter,Pointer(Result.Id));
  _CheckBox(Result.Node);

  OutputDebugString(PChar(Format('Inserting %s Index %d, Id %d',[Result.Caption,Result.Index,Result.Id])));  

  EndUpdate;

//VirtualTree.Paint;
  VirtualTree.InvalidateNode(Result.Node);
end;

//================== TVirtualListViewItems ==================================

function TVirtualListViewItems.AddNode(Node: TVirtualListViewItem;
  const S: string): TVirtualListViewItem;
begin
  BeginUpdate;

  Result := TVirtualListViewItem(inherited Add);
  Result.Caption:=s;

  if Assigned(VirtualTree)
    then
      begin
        if Assigned(Node)
          then Result.Node:=VirtualTree.InsertNode(Node.Node,amInsertAfter,Pointer(Result.Id))
          else Result.Node:=VirtualTree.InsertNode(nil,amInsertAfter,Pointer(Result.Id));
        _CheckBox(Result.Node);
      end;

  OutputDebugString(Pchar(Format('AddNode(%d)',[Result.Index])));

  EndUpdate;
end;

function TVirtualListViewItems.AddChild(Node: TVirtualListViewItem;
  const S: string): TVirtualListViewItem;
begin
  BeginUpdate;

  Result := TVirtualListViewItem(inherited Add);

  if Assigned(VirtualTree)
    then
      begin
        if Assigned(Node)
          then Result.Node:=VirtualTree.AddChild(Node.Node,Pointer(Result.Id))
          else Result.Node:=VirtualTree.AddChild(nil,Pointer(Result.Id));
        _CheckBox(Result.Node);
      end;

  Result.Caption:=s;

  OutputDebugString(Pchar(Format('AddChild(%d)',[Result.Index])));

  EndUpdate;
end;


function TVirtualListViewItems.AddChildFirst(Node: TVirtualListViewItem;
  const S: string): TVirtualListViewItem;
begin
  BeginUpdate;

  Result := Add();
  Result.Caption:=s;
  if Assigned(Node)
    then Result.Node:=VirtualTree.InsertNode(Node.Node,amAddChildFirst,Pointer(Result.Id))
    else Result.Node:=VirtualTree.InsertNode(nil,amAddChildFirst,Pointer(Result.Id));
  _CheckBox(Result.Node);

  OutputDebugString(Pchar(Format('AddChildFirst(%d)',[Result.Index])));

  EndUpdate;
end;


function TVirtualListViewItems.AddFirst(Node: TVirtualListViewItem;
  const S: string): TVirtualListViewItem;
begin
  BeginUpdate;

  Result := TVirtualListViewItem(inherited Add);
  Result.Caption:=s;
  if Assigned(Node)
    then Result.Node:=VirtualTree.InsertNode(Node.Node^.Parent,amAddChildFirst,Pointer(Result.Id))
    else Result.Node:=VirtualTree.InsertNode(nil,amAddChildFirst,Pointer(Result.Id));
  _CheckBox(Result.Node);

  OutputDebugString(Pchar(Format('AddFirst(%d)',[Result.Index])));

  EndUpdate;
end;


procedure TVirtualListViewItems.DeleteNode(Node: TVirtualListViewItem);
begin
  Delete(Node.Index);
end;


function TVirtualListViewItems.GetFirstNode: TVirtualListViewItem;
var
  ndx : Integer;
begin
  ndx:=Integer(VirtualTree.GetNodedata(VirtualTree.GetFirst)^);
  Result:=TVirtualListViewItem(FindItemID(ndx));
end;


function TVirtualListViewItems.Insert(Node: TVirtualListViewItem;
  const S: string): TVirtualListViewItem;
begin
  Result:=Insert(FindItemID(Node.Id).Index);
  Result.Caption:=s;

  VirtualTree.InvalidateNode(Result.Node);
end;


//======================= TVirtualListViewItems ======================================

procedure TVirtualListViewItems.Clear;
begin
  BeginUpdate;
  VirtualTree.Clear;
  EndUpdate;
  inherited;
end;

procedure TVirtualListViewItems._CheckBox(aNode : PVirtualNode);
begin
  if (toCheckSupport in TStringTreeOptions(FVirtualTree.TreeOptions).MiscOptions)
    then aNode^.CheckType:=ctTriStateCheckBox
    else aNode^.CheckType:=ctNone;
end;

function TVirtualListViewItems.GetItem(
  Index: Integer): TVirtualListViewItem;
begin
  Result := TVirtualListViewItem(inherited GetItem(Index));
end;

procedure TVirtualListViewItems.SetItem(Index: Integer; Value: TVirtualListViewItem);
begin
  inherited SetItem(Index, Value);
end;

function TVirtualListViewItems._Locate(const aNode: PVirtualNode): Integer;
var
  ndx : Integer;
begin
  ndx:=Integer(FVirtualTree.GetNodedata(aNode)^);
  Result:=FindItemID(ndx).Index;
end;

//======================== TVirtualStringTreeData ==================================

constructor TVirtualStringTreeData.Create(AOwner: TComponent);
begin
  inherited;

  NodeDataSize:=Sizeof(Integer);
  
  FVirtualListViewItems:=TVirtualListViewItems.Create(Self,TVirtualListViewItem);
  FVirtualListViewItems.VirtualTree:=Self;
end;


destructor TVirtualStringTreeData.Destroy;
begin
  FreeAndNil(FVirtualListViewItems);

  inherited;
end;

function TVirtualStringTreeData.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
begin
  Result:=0;
  if Assigned(FOnCompareEvent)
    then FOnCompareEvent(Self,Items[Items._Locate(Node1)],Items[Items._Locate(Node2)],Column,Result);
end;

function TVirtualStringTreeData.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
                          var Ghosted: Boolean; var Index: Integer): TCustomImageList;

var
  ndx : Integer;
begin
  if Column=0
    then
      begin
        ndx:=Data._Locate(Node);
        if (ndx>=0) and (ndx<Data.Count) and Assigned(Data.Items[ndx])
          then Index:=Data.Items[ndx].ImageIndex;
        Ghosted:=False;
      end;
end;

procedure TVirtualStringTreeData.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: String);
var
  ndx : Integer;
begin
  ndx:=Data._Locate(Node);
  if (ndx>=0) and (ndx<Data.Count) and Assigned(Data.Items[ndx])
    then
      begin
      //OutputDebugString(PChar(Format('Row%d,Col%d',[ndx,Column])));
        if (Column<=0)
          then
{$IFDEF DEBUG}
            Text:=Format(Data.Items[ndx].Caption+':Index=%d,Id%d',[ndx,Data.Items[ndx].Id])
{$ELSE  DEBUG}
            Text:=Data.Items[ndx].Caption
{$ENDIF DEBUG}
          else
            begin
              if Assigned(FVirtualListViewItems.Items[ndx].SubItems) and
                 (Column<=FVirtualListViewItems.Items[ndx].SubItems.Count)
                then
{$IFDEF DEBUG}
                Text:=Format(FVirtualListViewItems.Items[ndx].SubItems[Column-1]+':Index=%d,Id%d',[ndx,Data.Items[ndx].Id])
{$ELSE  DEBUG}
                Text:=FVirtualListViewItems.Items[ndx].SubItems[Column-1]
{$ENDIF DEBUG}
                else Text:='';
            end
      end;
end;

{
procedure TVirtualStringTreeData.SetCheckBoxes(const Value: Boolean);
var
  aNode: PVirtualNode;
  ct: TCheckType;
begin
  ct:=ctNone; //To get Rid of Warning..
  case value of
    True : ct:=ctCheckBox;
    False: ct:=ctNone;
  end;

  case value of
    True : TStringTreeOptions(TreeOptions).MiscOptions:=TStringTreeOptions(TreeOptions).MiscOptions+[toCheckSupport];
    False: TStringTreeOptions(TreeOptions).MiscOptions:=TStringTreeOptions(TreeOptions).MiscOptions-[toCheckSupport];
  end;

  aNode:=GetFirst;
  if Assigned(aNode)
    then
      while (aNode<>nil) do
        begin
          CheckType[aNode]:=ct;
          aNode:=GetNext(aNode);
        end;
end;
}

function TVirtualStringTreeData.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;


end.
