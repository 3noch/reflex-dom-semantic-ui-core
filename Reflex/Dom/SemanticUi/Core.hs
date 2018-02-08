{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}

module Reflex.Dom.SemanticUi.Core where

import           Data.Monoid         ((<>))
import qualified GHCJS.DOM.Element   as DOM
import           GHCJS.DOM.Types     (MonadJSM, JSM, JSVal, JSString, liftJSM, fromJSString, fromJSValUnchecked)
import           Reflex.Dom.Core     hiding (fromJSString)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)

#ifdef ghcjs_HOST_OS
import           GHCJS.DOM.Types        (pFromJSVal)
import           GHCJS.Foreign.Callback (Callback, asyncCallback1)
#else
import           Control.Monad                      (void)
--import Control.Concurrent (forkIO, threadDelay)
--import Control.Monad      (unless)
import           Control.Lens.Operators             ((^.))
--import Language.Javascript.JSaddle.Monad (askJSM, runJSM)
import           Language.Javascript.JSaddle.Object (js1, js2, jsg, jsg1, jss, obj, fun)
#endif

-- Constraints needed by these FFI functions.
type SemUiFfiConstraints t m = (
  DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadHold t m,
  MonadJSM (Performable m), PostBuild t m, PerformEvent t m, TriggerEvent t m
  )

-- DROPDOWN ---------------------------------------------------------

newtype SemDropdownConfig = SemDropdownConfig{
  _semDropdownConfig_action :: DropdownAction  -- The action taken when a user chooses an item.
  }

newtype SemDropdown t = SemDropdown{
  _semDropdown_change :: Event t Text
  -- ^ Event firing when the dropdown changes. Multiple selections will be comma-delimited.
  }

data DropdownAction
  = DropdownActionActivate
  | DropdownActionCombo
  | DropdownActionSelect
  | DropdownActionHide
  | DropdownActionNothing
  -- DropdownActionFunction
  deriving (Bounded, Enum)

dropdownActionToSemUi :: DropdownAction -> JSString
dropdownActionToSemUi x = case x of
  DropdownActionActivate -> "activate"
  DropdownActionCombo    -> "combo"
  DropdownActionSelect   -> "select"
  DropdownActionHide     -> "hide"
  DropdownActionNothing  -> "nothing"


activateSemUiDropdownEl :: DOM.Element -> JSString -> (JSVal -> JSM ()) -> JSM ()
#ifdef ghcjs_HOST_OS

activateSemUiDropdownEl e action onChange = do
  cb <- asyncCallback1 $ onChange . pFromJSVal
  js_activateSemUiDropdownEl e action cb

-- TODO: This mounting loop is a hack.
foreign import javascript unsafe
  "var opts = {}; \
  opts['action']   = $2; \
  opts['onChange'] = function(value, text, elem) { \
    $3(value || ''); \
  }; \
  function mountWhenReady() { \
    if (jQuery.contains(document, $1)) { \
      jQuery($1)['dropdown'](opts); \
    } else { \
      setTimeout(mountWhenReady, 100); \
    } \
  } \
  mountWhenReady();"
  js_activateSemUiDropdownEl :: DOM.Element -> JSString -> Callback (JSVal -> JSM ()) -> JSM ()

#else

activateSemUiDropdownEl e action onChange = do
  o <- obj
  o ^. jss (t_ "action") action
  o ^. jss (t_ "onChange") (fun $ \_ _ args -> case args of
    (val:_) -> onChange val
    _ -> error "Semantic UI dropdown onChange event did not send the expected number of arguments"
    )

  doc <- jsg (t_ "document")

  let
    tryMount = do
      isReady' <- jsg (t_ "jQuery") ^. js2 (t_ "contains") doc e
      isReady :: Bool <- fromJSValUnchecked isReady'
      void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "dropdown") o
      pure isReady

  void tryMount

  -- NOTE: Using this retry stuff works, but it can't go fast enough so we'll just use a "safe" delay (see below).
  --let
  --   mountWhenReady = do
  --     liftIO $ threadDelay 1000000 -- 1 second. NOTE: JSaddle warp cannot go any faster than this or it crashes.
  --     success <- tryMount
  --     unless success mountWhenReady
  --     pure ()

  -- done <- tryMount

  -- unless done $ do
  --   ctx <- askJSM
  --   void $ liftIO $ forkIO $ do
  --     runJSM mountWhenReady ctx

-- " -- this is here to fix syntax highlighting!
#endif


mkSemUiDropdown
  :: (SemUiFfiConstraints t m)
  => Element EventResult (DomBuilderSpace m) t -> SemDropdownConfig -> m (SemDropdown t)
mkSemUiDropdown el_ config = do
  -- Setup the event and callback function for when the value is changed
  (onChangeEvent, onChangeCallback) <- newTriggerEvent

#ifdef ghcjs_HOST_OS
  -- In JavaScript we use a 'setTimeout' to ensure the attach happens.
  attachEv <- getPostBuild
#else
  -- In JSaddle we just pick a "safe" delay and hope it works!
  attachEv <- delay 0.2 =<< getPostBuild
#endif

  -- Activate the dropdown after build
  performEvent_ $ ffor attachEv $ \() -> liftJSM $
    activateSemUiDropdownEl
      (_element_raw el_)
      (dropdownActionToSemUi $ _semDropdownConfig_action config)
      (\vals -> do
        txts <- fromJSValUnchecked vals
        liftIO (onChangeCallback txts)
      )

  pure $ SemDropdown onChangeEvent


-- ACCORDION --------------------------------------------------------

newtype AccordionConfig = AccordionConfig{
  _accordionConfig_exclusive :: Bool  -- ^ Whether or not the accordion allows only one item open at a time.
  }

newtype Accordion t = Accordion{
  _accordion_opened :: Event t Text  -- ^ Event firing when the accordion opens.
  }

activateSemUiAccordionEl :: DOM.Element -> Bool -> (JSString -> JSM ()) -> JSM ()
#ifdef ghcjs_HOST_OS

activateSemUiAccordionEl e exclusive onOpen = do
  cb <- asyncCallback1 $ onOpen . pFromJSVal
  js_activateSemUiAccordionEl e exclusive cb

foreign import javascript unsafe
  "var opts = {}; \
  opts['exclusive'] = $2; \
  opts['onOpen'] = function() { \
    $3(jQuery($1)['find']('[data-accordion-id].active')['data']('accordion-id') || ''); \
  }; \
  jQuery($1)['accordion'](opts);"
  js_activateSemUiAccordionEl :: DOM.Element -> Bool -> Callback (JSVal -> JSM ()) -> JSM ()

#else

activateSemUiAccordionEl e exclusive onOpen = do
  o <- obj
  o ^. jss (t_ "exclusive") exclusive
  o ^. jss (t_ "onOpen") (fun $ \_ _ _ -> do
    activeEl <- jsg1 (t_ "jQuery") e ^. js1 (t_ "find") (t_ "[data-accordion-id].active")
    accordionId <- activeEl ^. js1 (t_ "data") (t_ "accordion-id") -- TODO: Add the '|| ""' bit to this like it is in the FFI.
    onOpen =<< fromJSValUnchecked accordionId)
  void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "accordion") o

-- " -- this is here to fix syntax highlighting!
#endif

-- | Initializes a Semantic-UI @accordion@ on the given element.
--
-- This hook uses the @data-accordion-id@ attribute to tell you which accordion receives action.
-- Each item in the accordion must have this attribute.
mkSemUiAccordion
  :: SemUiFfiConstraints t m
  => Element EventResult (DomBuilderSpace m) t
  -> AccordionConfig
  -> m (Accordion t)
mkSemUiAccordion el_ config = do
  -- Setup the event and callback function for when the accordion is opened
  (onOpenEvent, onOpenCallback) <- newTriggerEvent

  -- Activate the accordion after build
  schedulePostBuild $ liftJSM $
    activateSemUiAccordionEl
      (_element_raw el_)
      (_accordionConfig_exclusive config)
      (\accordionId -> liftIO (onOpenCallback $ fromJSString accordionId))

  pure $ Accordion onOpenEvent


-- RATING -----------------------------------------------------------

data RatingConfig = RatingConfig{
  _ratingConfig_clearable   :: !Bool, -- ^ Can the rating be cleared?
  _ratingConfig_interactive :: !Bool  -- ^ Can the user change the rating?
  }

newtype Rating t = Rating{
  _rating_change :: Event t Int  -- ^ Event firing when the rating changes.
  }

activateSemUiRatingEl :: DOM.Element -> RatingConfig -> (JSVal -> JSM ()) -> JSM ()
#ifdef ghcjs_HOST_OS

activateSemUiRatingEl e cfg onRate = do
  cb <- asyncCallback1 $ onRate . pFromJSVal
  js_activateSemUiRatingEl
    e
    (_ratingConfig_clearable cfg)
    (_ratingConfig_interactive cfg)
    cb

foreign import javascript unsafe
  "var opts = {}; \
  opts['clearable']   = $2; \
  opts['interactive'] = $3; \
  opts['onRate']      = function(val) { $4(val || 0); }; \
  jQuery($1)['rating'](opts);"
  js_activateSemUiRatingEl :: DOM.Element -> Bool -> Bool -> Callback (JSVal -> JSM ()) -> JSM ()
-- " -- this is here to fix syntax highlighting!

#else

activateSemUiRatingEl e cfg onRate = do
  o <- obj
  o ^. jss (t_ "clearable")   (_ratingConfig_clearable cfg)
  o ^. jss (t_ "interactive") (_ratingConfig_interactive cfg)
  o ^. jss (t_ "onRate") (fun $ \_ _ args -> case args of
    (val:_) -> onRate val
    _ -> error "Semantic UI rating onRate event did not send the expected number of arguments"
    )

  void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "rating") o

#endif


mkSemUiRating :: (SemUiFfiConstraints t m) => RatingConfig -> Element EventResult (DomBuilderSpace m) t -> m (Rating t)
mkSemUiRating cfg el_ = do
  -- Setup the event and callback function for when the value is changed
  (onRateEvent, onRateCallback) <- newTriggerEvent

  -- Activate the dropdown after build
  schedulePostBuild $ liftJSM $
    activateSemUiRatingEl
      (_element_raw el_)
      cfg
      (\jsVal -> do
        val <- fromJSValUnchecked jsVal
        liftIO (onRateCallback val)
      )
  pure $ Rating onRateEvent


-- MODAL -----------------------------------------------------------

data ModalBehavior
  = ShowModal
  | HideModal
  | ToggleModal
  | RefreshModal
  | ShowDimmer
  | HideDimmer
  | HideOthers
  | HideAll
  | CacheSizes
  | SetActive
  deriving (Bounded, Eq, Enum, Show)

modalBehaviorString :: ModalBehavior -> JSString
modalBehaviorString beh = case beh of
  ShowModal    -> "show"
  HideModal    -> "hide"
  ToggleModal  -> "toggle"
  RefreshModal -> "refresh"
  ShowDimmer   -> "show dimmer"
  HideDimmer   -> "hide dimmer"
  HideOthers   -> "hide others"
  HideAll      -> "hide all"
  CacheSizes   -> "cache sizes"
  SetActive    -> "set active"

------------------------------------------------------------------------------
uiModal :: SemUiFfiConstraints t m => Text -> Event t ModalBehavior -> m a -> m a
uiModal classes_ beh children = do
  (e,res) <- elAttr' "div" ("class"=:("ui "<>classes_<>" modal")) children
  performEvent_ (liftJSM . uiTriggerModalAction (_element_raw e) <$> beh)
  pure res

------------------------------------------------------------------------------
uiTriggerModalAction :: DOM.Element -> ModalBehavior -> JSM ()
uiTriggerModalAction e beh = js_modalAction e (modalBehaviorString beh)

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "jQuery($1)['modal']($2);"
  js_modalAction :: DOM.Element -> JSString -> IO ()
-- " -- this is here to fix syntax highlighting!

#else

js_modalAction :: DOM.Element -> JSString -> JSM ()
js_modalAction e beh =
  void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "modal") beh

#endif


-- CHECKBOX ---------------------------------------------------------

activateCheckbox :: DOM.Element -> (Bool -> JSM ()) -> JSM ()
#ifdef ghcjs_HOST_OS

activateCheckbox e onChange = do
  cb <- asyncCallback1 $ onChange . pFromJSVal
  js_activateCheckbox e cb
foreign import javascript unsafe
  "var opts = {}; \
  opts['onChange'] = function() { \
    $2(jQuery($1)['checkbox']('is checked')); \
  }; \
  jQuery($1)['checkbox'](opts);"
  js_activateCheckbox :: DOM.Element -> Callback (JSVal -> JSM ()) -> JSM ()

#else

activateCheckbox e onChange = do
  o <- obj
  o ^. jss (t_ "onChange") (fun $ \_ _ _ -> do
    isChecked <- jsg1 (t_ "jQuery") e ^. js1 (t_ "checkbox") (t_ "is checked")
    onChange =<< fromJSValUnchecked isChecked)
  void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "checkbox") o

#endif


#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
  "jQuery($1)['checkbox']($2);"
  setCheckboxState :: DOM.Element -> JSString -> JSM ()
#else

setUiCheckboxState  :: DOM.Element -> JSString -> JSM ()
setUiCheckboxState e newState
  = void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "checkbox") newState
#endif

--------------------------------------------------------------------------------

data UiCheckboxState
  = UiCheckboxToggle
  | UiCheckboxCheck
  | UiCheckboxUncheck
  | UiCheckboxIndeterminate
  | UiCheckboxDeterminate
  | UiCheckboxEnable
  | UiChekboxDisable
  deriving (Bounded, Enum, Eq, Show)


uiCheckboxStateString :: UiCheckboxState -> JSString
uiCheckboxStateString x = case x of
  UiCheckboxToggle        -> "toggle"
  UiCheckboxCheck         -> "check"
  UiCheckboxUncheck       -> "uncheck"
  UiCheckboxIndeterminate -> "indeterminate"
  UiCheckboxDeterminate   -> "determinate"
  UiCheckboxEnable        -> "enable"
  UiChekboxDisable        -> "disable"

uiCheckbox
  :: (SemUiFfiConstraints t m)
  => Element EventResult (DomBuilderSpace m) t
  -> m (Event t Bool)
uiCheckbox el_ = do
  (onChangeEvent, onChangeCallback) <- newTriggerEvent

  schedulePostBuild $ liftJSM $
    activateCheckbox (_element_raw el_) $ liftIO . onChangeCallback

  pure onChangeEvent


-- EMBED ------------------------------------------------------------

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
  "jQuery($1)['embed']();"
  activateEmbed :: DOM.Element -> JSM ()

#else

activateEmbed :: DOM.Element -> JSM ()
activateEmbed e = do
  o <- obj
  void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "embed") o

#endif

uiEmbed
  :: (SemUiFfiConstraints t m)
  => Element EventResult (DomBuilderSpace m) t
  -> m ()
uiEmbed el_ = schedulePostBuild $ liftJSM $ activateEmbed (_element_raw el_)


-- SHAPE ------------------------------------------------------------

data ShapeBehavior
  = ShapeFlipUp
  | ShapeFlipDown
  | ShapeFlipLeft
  | ShapeFlipRight
  | ShapeFlipOver
  | ShapeFlipBack
  | ShapeSetDefaultSide
  | ShapeSetStageSize
  | ShapeReset
  | ShapeRepaint
  | ShapeRefresh
  deriving (Bounded, Eq, Enum, Show)

shapeBehaviorString :: ShapeBehavior -> JSString
shapeBehaviorString beh = case beh of
  ShapeFlipUp         -> "flip up"
  ShapeFlipDown       -> "flip down"
  ShapeFlipLeft       -> "flip left"
  ShapeFlipRight      -> "flip right"
  ShapeFlipOver       -> "flip over"
  ShapeFlipBack       -> "flip back"
  ShapeSetDefaultSide -> "set default side"
  ShapeSetStageSize   -> "set stage size"
  ShapeReset          -> "reset"
  ShapeRepaint        -> "repaint"
  ShapeRefresh        -> "refresh"

------------------------------------------------------------------------------
uiShape
  :: SemUiFfiConstraints t m
  => Element EventResult (DomBuilderSpace m) t
  -> Event t ShapeBehavior
  -> m ()
uiShape e beh = performEvent_ (liftJSM . uiTriggerShapeAction (_element_raw e) <$> beh)


------------------------------------------------------------------------------
uiTriggerShapeAction :: DOM.Element -> ShapeBehavior -> JSM ()
uiTriggerShapeAction e beh = js_shapeAction e (shapeBehaviorString beh)

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "jQuery($1)['shape']($2);"
  js_shapeAction :: DOM.Element -> JSString -> IO ()
-- " -- this is here to fix syntax highlighting!

#else

js_shapeAction :: DOM.Element -> JSString -> JSM ()
js_shapeAction e beh =
  void $ jsg1 (t_ "jQuery") e ^. js1 (t_ "shape") beh

#endif



-- Helpers ----------------------------------------------------------
t_ :: Text -> Text
t_ = id
