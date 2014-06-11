module MainWindow where

import Graphics.UI.Gtk

import Data.Tree

{- | This module describes the main window of the application using
     the standard GTK library widgets

  Sketch of the main window structure:

  Window
    VBox
      Toolbar
        New  ToolButton
        Open ToolButton
        Save ToolButton
      HBox
        TreeView (for selecting type graph, graph, rules, etc.)
        Image (for showing the SVG output)      
-}


-- Test interface of application 

test = do

  -- Init GTK 
  initGUI

  -- Create and configure main window
  window <- windowNew
  set window [windowTitle:="VeriGraph",
              windowDefaultWidth:=620,
              windowDefaultHeight:=600]
   
  -- Create main vertical box
  vboxMain <- vBoxNew False 0
  containerAdd window vboxMain
  
  -- Create toolbar and standard toolbuttons
  toolbar1 <- toolbarNew
  toolbarSetStyle toolbar1 ToolbarIcons
  tb1 <- toolButtonNewFromStock stockNew
  tb2 <- toolButtonNewFromStock stockOpen
  tb3 <- toolButtonNewFromStock stockSave
  toolbarInsert toolbar1 tb1 0
  toolbarInsert toolbar1 tb2 1
  toolbarInsert toolbar1 tb3 2
  boxPackStart vboxMain toolbar1 PackNatural 5

  -- Create internal horizontal box
  hboxMain <- hBoxNew False 0
  boxPackStart vboxMain hboxMain PackNatural 5

  -- BEGIN OF TREEVIEW

  -- GTK2HS has a VERY bureaucratic tree widget manipulation API (unfortunately...)
  -- Quick explanation: 
  --      TreeView is the graphical widget, 
  --               it contains one or more Columns,
  --                 each associated with a Renderer 
  --                    (which specifies how to draw the stored elements)
  --   
  --      TreeStore is the data structure (a parameterized rose tree)
  -- 
  --      The connection between the TreeStore, Column and Renderer is done
  --      by means of the cellLayoutSetAttributes call

  -- model containing the data (rose tree of strings) 
  ts   <- treeStoreNew [Node "gg1" 
                          [Node "tg" [], 
                           Node "g0" [], 
                           Node "rule1" []], 
                        Node "gg2" []]
  -- widget for visualization of ts
  tv   <- treeViewNewWithModel ts
  boxPackStart hboxMain tv PackNatural 5
  -- create a column in tv, and add it to the widget
  col  <- treeViewColumnNew
  treeViewAppendColumn tv col
  -- create a renderer to show each element stored in ts graphically in col
  rend <- cellRendererTextNew 
  cellLayoutPackStart col rend False
  -- connects column, renderer and model
  cellLayoutSetAttributes col rend ts (\v -> [cellText := v,cellTextEditable := True])

  -- on edition of the text, keep changes
  rend `on` edited $ \path str -> treeStoreSetValue ts path str 
            
  -- END OF TREEVIEW
   


  -- Callbacks
  onDestroy window  $ mainQuit

  -- Default visibility of window
  widgetShowAll window  
 
  -- Run main loop
  mainGUI
