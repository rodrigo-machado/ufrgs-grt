{-# LANGUAGE ScopedTypeVariables #-}

module MainWindow where

-- Common imports
import Graphics.UI.Gtk
import Data.Tree
import Data.Maybe
-- Exception handling
import Control.Exception 
-- Needed to safely read text from/to files
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
  tbOpen <- toolButtonNewFromStock stockOpen
  tbSave <- toolButtonNewFromStock stockSave
  toolbarInsert toolbar1 tbOpen 0
  toolbarInsert toolbar1 tbSave 1
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
  treeViewColumnSetTitle col "Graph Grammars"
  treeViewAppendColumn tv col
  -- create a renderer to show each element stored in ts graphically in col
  rend <- cellRendererTextNew 
  cellLayoutPackStart col rend True
  -- connects column, renderer and model
  cellLayoutSetAttributes col rend ts (\v -> [cellText := v,cellTextEditable := True])

  -- on edition of the text, keep changes
  rend `on` edited $ \path str -> treeStoreSetValue ts path str 
            
  -- END OF TREEVIEW
   
  -- Temporary output label
  label <- labelNew $ Just "Ola"
  boxPackStart hboxMain label PackNatural 5


  -- Create Open and Close Dialogues associated with the main window
  openDialog <- fileChooserDialogNew 
                 (Just "Open file")    -- Title
                 (Just window)         -- Main window
                 FileChooserActionOpen -- Save/Open customization
                 [("Cancel",ResponseCancel),("Open",ResponseAccept)] -- Buttons and actions
  -- Starts hidden
  widgetHide openDialog

  saveDialog <- fileChooserDialogNew 
                 (Just "Save file")    -- Title
                 (Just window)         -- Main window
                 FileChooserActionSave -- Save/Open customization
                 [("Cancel",ResponseCancel),("Save",ResponseAccept)] -- Buttons and actions
  -- Check before overwriting files
  fileChooserSetDoOverwriteConfirmation saveDialog True
  -- Starts hidden
  widgetHide saveDialog



  -- Callbacks
  onDestroy window  $ mainQuit

  onToolButtonClicked tbOpen $ do
     str <- openFileAndGetContents openDialog
     set label [labelText := fromMaybe "error" str]

  onToolButtonClicked tbSave $ do
     str <- get label labelText 
     saveStringAsFile saveDialog str



  -- Default visibility of window
  widgetShowAll window  
 
  -- Run main loop
  mainGUI


------------------------------- GUI Helpers -----------------------------------------


-------------------- show alert messages to the user -----------------------------
alert :: String -> IO ()
alert s = do dia <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk s
             dialogRun dia
             widgetDestroy dia
             return ()

-------------------- reading files as strings ---------------------------

openFileAndGetContents :: FileChooserDialog -> IO (Maybe String)
openFileAndGetContents openDialog = do
    response <- dialogRun openDialog
    case response of
      ResponseAccept -> do
        filename <- fileChooserGetFilename openDialog
        case filename of
          Nothing -> do 
             alert "Please enter a file name!"
             widgetHide openDialog
             return Nothing
          Just path -> 
             catch 
               (do t <- TIO.readFile path
                   widgetHide openDialog
                   return $ Just $ T.unpack t)
               (\e -> do 
                      alert (show (e::SomeException))                   
                      widgetHide openDialog
                      return $ Nothing)             
      _  -> do 
        widgetHide openDialog
        return $ Nothing


-------------------- writing strings as files ---------------------------


-- -- Salva arquivo
saveStringAsFile :: FileChooserDialog -> String -> IO ()
saveStringAsFile saveDialog str = do
     response <- dialogRun saveDialog
     case response of
       ResponseAccept -> do
         filename <- fileChooserGetFilename saveDialog
         case filename of
           Nothing -> do
             alert "No file written"
             widgetHide saveDialog
             return ()
           Just path -> do 
             catch  
               (do TIO.writeFile path $ T.pack str
                   widgetHide saveDialog)
               (\e -> do 
                      alert (show (e::SomeException))                   
                      widgetHide saveDialog
                      return ())
 
       _ -> widgetHide saveDialog

