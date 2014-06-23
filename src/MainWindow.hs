{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- Common imports
import Graphics.UI.Gtk
import qualified Data.Tree as TR
import Data.Maybe
import Data.IORef
-- Exception handling
import Control.Exception 
-- Run external processes
import System.Process
-- Needed to safely read text from/to files
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Parser/Printer
import Graph.Serialized
import Graph.Digraph
import Graph.Rewriting
import Graph.StateSpace
import Assorted.Render

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

--- Internal element for treeStore manipulation of Graph Grammars

data GGElem a b = 
       GraphGrammar  String
     | TypeGraph     String (TypedDigraph a b)   -- type graph
     | InstanceGraph String (TypedDigraph a b)   -- instance graph
     | GraphRule     String (Rule a b) 
--     deriving (Eq,Ord,Show,Read)                   

nameOf (GraphGrammar s)    = s
nameOf (TypeGraph s t)     = s
nameOf (InstanceGraph s i) = s
nameOf (GraphRule s r)     = s

setName s (GraphGrammar s')    = GraphGrammar s
setName s (TypeGraph s' t)     = (TypeGraph s t)
setName s (InstanceGraph s' i) = (InstanceGraph s i)
setName s (GraphRule s' r)     = (GraphRule s r)

-- Test interface of application 

main = do

  -- Init GTK 
  initGUI

  -- Create and configure main window
  window <- windowNew
  set window [windowTitle:="VeriGraph",
              windowDefaultWidth:=480,
              windowDefaultHeight:=240]
   
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
  ts   <- treeStoreNew [TR.Node (GraphGrammar "gg1") []]
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
  cellLayoutSetAttributes col rend ts (\ggelem -> [cellText := nameOf ggelem,cellTextEditable := True])

  -- on edition of the text, keep changes
  rend `on` edited $ \path str -> do ggelem <- treeStoreGetValue ts path 
                                     treeStoreSetValue ts path (setName str ggelem)
            
  -- END OF TREEVIEW
   

  -- GUI state: an IORef containing an empty Graph Grammar
  stateGG <- newIORef $ (Serialized [] [] :: Serialized () ())


  -- Temporary widgets for testing only

  label <- labelNew $ Just "Ola"
  boxPackStart hboxMain label PackNatural 5
  
  button <- buttonNewWithLabel "Call dot"
  boxPackStart hboxMain button PackNatural 5

  img    <- imageNew 
  boxPackStart hboxMain img PackNatural 5


  -- Create Open and Close Dialogues associated with the main window
  openDialog <- fileChooserDialogNew 
                 (Just "Open file")    -- Title
                 (Just window)         -- Main window
                 FileChooserActionOpen -- Save/Open customization
                 [("Cancel",ResponseCancel),("Open",ResponseAccept)] -- Buttons and actions
  widgetHide openDialog   -- Starts hidden

  saveDialog <- fileChooserDialogNew 
                 (Just "Save file")    -- Title
                 (Just window)         -- Main window
                 FileChooserActionSave -- Save/Open customization
                 [("Cancel",ResponseCancel),("Save",ResponseAccept)] -- Buttons and actions
  -- Check before overwriting files
  fileChooserSetDoOverwriteConfirmation saveDialog True
  widgetHide saveDialog   -- Starts hidden


  -- Callbacks
  onDestroy window $ mainQuit

  onToolButtonClicked tbOpen $ do
     -- read file content
     str <- openFileAndGetContents openDialog
     -- convert it to a graph grammar
     case str of
       Nothing -> 
         alert "Error loading graph grammar"
       Just str' -> do
         putStrLn $ "olha => " ++ str'
         catch 
           (writeIORef stateGG $ (unserialize str'))
           (\e -> alert $ show (e::SomeException))
         -- extract graph grammar
         Serialized gl rl <- readIORef stateGG
         -- fill treestore based on gg
         treeStoreClear ts
         treeStoreInsertTree ts [] 0 $ TR.Node (GraphGrammar "grammar") $ (map (\a->TR.Node a [])) $ (map (InstanceGraph "IG:") gl) ++ (map (GraphRule "RL:") rl)
         -- load it in the interface state
         ss <- runStateSpace 5 (head gl) rl
         let txt = finishDot "G" $ ggToDot ss
         --set label [labelText := dot ]
         readProcess "dot" ["-Tsvg", "-otmp.svg"] txt
         pb <- pixbufNewFromFileAtSize "tmp.svg" 600 600
         --pb <- pixbufNewFromFile "tmp.svg"
         set img [imagePixbuf := pb]


  onToolButtonClicked tbSave $ do
     str <- get label labelText 
     -- graph grammar as 
     saveStringToFile saveDialog str

  button `on` buttonActivated $ do 
     txt <- get label labelText
     readProcess "dot" ["-Tsvg", "-otmp.svg"] txt
     --imageSetFromFile img "tmp.svg"
     pb <- pixbufNewFromFileAtSize "tmp.svg" 600 600
     set img [imagePixbuf := pb]

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
saveStringToFile :: FileChooserDialog -> String -> IO ()
saveStringToFile saveDialog str = do
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

