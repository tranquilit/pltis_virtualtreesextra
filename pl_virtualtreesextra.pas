{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pl_virtualtreesextra;

interface

uses
  AllVTExtraRegister, vte_buttontree, vte_collectionvirtualtreemediator, 
  vte_comboeditlink, vte_configproviter, vte_controller, vte_dbcheckgroup, 
  vte_dbgrid, vte_dbtree, vte_dbtreeex, vte_ddbtreeview, vte_ddbtreeviewutils, 
  vte_edittree, vte_edittree_editors, vte_initree, vte_json, 
  vte_nodeinterfaces, vte_objecttree, vte_propertytree, vte_stringlist, 
  vte_treedata, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AllVTExtraRegister', @AllVTExtraRegister.Register);
end;

initialization
  RegisterPackage('pl_virtualtreesextra', @Register);
end.
