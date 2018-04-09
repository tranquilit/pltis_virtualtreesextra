{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit vte_initree;

{$mode delphi}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, IniFiles;

type
  TVirtualIniTree = class(TCustomVirtualStringTree)
  private
    fIniFile: String;
    fIni: TIniFile;
    procedure SetIniFile(const Value: String);
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
                      var Text: String); override;
  protected
    function GetOptionsClass: TTreeOptionsClass; override;
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
    property OnCompareNodes;
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
    {The Ini file shown.}
    property IniFile : String read fIniFile write SetIniFile;
  end;


implementation

{ TVirtualIniTree }

constructor TVirtualIniTree.Create(AOwner: TComponent);
begin
  inherited;

  Header.Columns.Clear;
  Header.Columns.Add.Text:='Name';
  Header.Columns.Add.Text:='Value';
  Header.Options:=Header.Options+[hoVisible];

  Header.Columns[0].Width:=150;
  Header.Columns[1].Width:=50;

//Preset some settings so it looks like a R/O Memo
  DefaultNodeHeight:=13;      //Measured against a plain TMemo so it looks equal to it.
  Header.Height    :=17;      //Measured against a TListView;

  with TStringTreeOptions(TreeOptions) do
    begin
      PaintOptions    :=PaintOptions    +[toShowRoot,toShowTreeLines,toShowButtons,toHideFocusRect];
      SelectionOptions:=SelectionOptions+[toFullRowSelect];
    end;

  TextMargin :=0;
end;

procedure TVirtualIniTree.SetIniFile(const Value: String);
var
  sl : TStringList;
begin
  if FileExists(Value)
    then
      begin
        fIniFile := Value;
        if Assigned(fIni) then fIni.Free;
        fIni:=TIniFile.Create(fIniFile);
        sl:=TStringList.Create;
        fIni.ReadSections(sl);
        RootNodeCount:=sl.Count;
        sl.Free;
      end
    else
      begin
        fIniFile:='';
        RootNodeCount:=0;
        fIni.Free;
        fIni:=nil;
      end;
end;

procedure TVirtualIniTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
                      var Text: String);
var
  s  : String;
  sl : TStringList;
begin
  inherited;

  sl:=TStringList.Create;
  case Column of

    0 : begin
          if GetNodeLevel(Node)=0
            then
              begin
                fIni.ReadSections(sl);
                s:=sl[Node^.Index];
                Text:='['+s+']';
                fIni.ReadSection(s,sl);
                if ChildCount[Node]<>Cardinal(sl.Count)
                  then ChildCount[Node]:=sl.Count;
              end
            else
              begin
                fIni.ReadSections(sl);
                fIni.ReadSection(sl[Node^.Parent^.Index],sl);
                Text:=sl[Node^.Index];
              end;
        end;

    1 : if (GetNodeLevel(Node)=0)
          then Text:=''
          else
            begin
              fIni.ReadSections(sl);
              s:=sl[Node^.Parent^.Index];
              fIni.ReadSection(s,sl);
              Text:=fIni.ReadString(s,sl[Node^.Index],'n/a')
            end;
  end;

  sl.Free;
end;

function TVirtualIniTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result := TStringTreeOptions;
end;


destructor TVirtualIniTree.Destroy;
begin
  SetLength(fIniFile,0);

  inherited;
end;

end.
