{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit vte_nodeinterfaces;

{$mode objfpc}{$H+}

interface

type

  IVirtualNode = interface
    function GetChildNode(Index: Cardinal): IVirtualNode;
    function GetNodeTitle: String;
    function GetChildNodeCount: Cardinal;
//    property NodeChecked: Boolean read GetNodeChecked;
    property NodeTitle: String read GetNodeTitle;
    property ChildNodeCount: Cardinal read GetChildNodeCount;
  end;

  IVirtualNodeList = interface
    function GetNode(Index: Cardinal): IVirtualNode;
    function GetNodeCount: Cardinal;
    property NodeCount: Cardinal read GetNodeCount;
  end;

implementation

end.

