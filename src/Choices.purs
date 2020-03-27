module Choices.Choices
  ( new
  , Option
  , Choices
  ) where

import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT)
import Data.Either (fromRight)
import Data.Eq (class Eq)
import Data.Hashable (class Hashable)
import Data.HashSet (HashSet)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Ord (class Ord)
import Data.Ordering (Ordering(..))
import Data.Unit (Unit)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign, readInt, readString)
import Foreign.Index ((!))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafePartial)
import Prelude (bind, (>>=), pure, ($))
import Web.Event.Event (EventType(..))
import Web.HTML.HTMLDivElement (HTMLDivElement)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLOptionElement (HTMLOptionElement)

type Value = String
type Label = String


foreign import data Choices :: Type -- main data type

foreign import data Choice :: Type

foreign import data Item :: Type

foreign import data PassedElement :: Type
-- https://github.com/jshjohnson/Choices/blob/68313da4122ec7f76274b22926e762914c96723d/src/scripts/interfaces.ts#L242

foreign import new :: HTMLElement -> HashSet Option -> Effect Choices
foreign import destroy :: Choices -> Effect Unit
foreign import init :: Choices -> Effect Unit
foreign import highlightAll :: Choices -> Effect Unit
foreign import unhighlightAll :: Choices -> Effect Unit
foreign import removeActiveItemsByValue :: Choices -> Value -> Effect Unit
foreign import removeActiveItems :: Choices -> Int -> Effect Unit
foreign import removeHighlightedItems :: Choices -> Effect Unit
foreign import showDropdown :: Choices -> Effect Unit
foreign import hideDropdown :: Choices -> Effect Unit
foreign import setChoices :: Choices -> Array Choice -> Value -> Label -> Boolean -> Effect Unit
-- Note: setChoices (choices :: Array Choice) not implemented as function even though choices.js supports this
foreign import clearChoices :: Choices -> Effect Unit
foreign import getValueValue :: Choices -> Effect Value
foreign import getValueObject :: Choices -> Effect Foreign
foreign import setValue :: Choices -> Array Item -> Effect Unit
foreign import setChoiceByValue :: Choices -> Array String -> Effect Unit
foreign import clearStore :: Choices -> Effect Unit
foreign import clearInput :: Choices -> Effect Unit
foreign import disable :: Choices -> Effect Unit
foreign import enable :: Choices -> Effect Unit

data OPosition
  = PosAuto
  | PosTop
  | PosBottom

data OAddItemFilter
  = StringOrRegex String
  | Function (Foreign -> Boolean)

data OSorter
  = SortByAlpha
  | SortByScore
  | SortByFunction (Foreign -> Foreign -> Ordering)

data ORenderSelectedChoices
  = RSC_Auto
  | RSC_Always

data OAddItemText
  = AIT_String String
  | AIT_Function (Foreign -> String)

data OCallbackOnCreateTemplates -- this is not finished, the are to become functions that take arguments
  = CT_ContainerOuter HTMLDivElement
  | CT_ContainerInner HTMLDivElement
  | CT_ItemList HTMLDivElement
  | CT_Placeholder HTMLDivElement
  | CT_Item HTMLDivElement
  | CT_ChoiceList HTMLDivElement
  | CT_ChoiceGroup HTMLDivElement
  | CT_Choice HTMLDivElement
  | CT_Input HTMLInputElement
  | CT_Dropdown HTMLDivElement
  | CT_Notice HTMLDivElement
  | CT_Option HTMLOptionElement

data Option
  = Silent Boolean
  | Items (Array Foreign)
  | Choices (Array Foreign)
  | RenderChoiceLimit Int
  | MaxItemCount Int
  | AddItems Boolean
  | RemoveItems Boolean
  | RemoveItemButton Boolean
  | EditItems Boolean
  | DuplicateItemsAllowed Boolean
  | Delimiter String
  | Paste Boolean
  | SearchEnabled Boolean
  | SearchChoices Boolean
  | SearchFields Foreign
  | SearchFloor Int
  | SearchResultLimit Int
  | Position OPosition
  | ResetScrollPosition Boolean
  | AddItemFilter (Maybe OAddItemFilter)
  | ShouldSort Boolean
  | ShouldSortItems Boolean
  | Sorter OSorter
  | Placeholder Boolean
  | PlaceholderValue (Maybe String)
  | SearchPlaceholderValue (Maybe String)
  | PrependValue (Maybe String)
  | AppendValue (Maybe String)
  | RenderSelectedChoices ORenderSelectedChoices
  | LoadingText String
  | NoResultsText String -- Not supporting the function here because no arguments are passed to it
  | NoChoicesText String -- Not supporting the function here because no arguments are passed to it
  | ItemSelectText String
  | AddItemText OAddItemText
  | MaxItemText String -- Not supporting a function here because the max item count can be grapped from the configuration
  | ValueComparer (Foreign -> Foreign -> Ordering)
  -- | CallbackOnInit -- The construction of the Choices is controlled here in this FFI module, so not supporting this for now
  | CallbackOnCreateTemplates (Array OCallbackOnCreateTemplates)

instance hashableOption :: Hashable Option where
  hash (Silent _) = 1
  hash (Items _) = 2
  hash (Choices _) = 3
  hash (RenderChoiceLimit _) = 4
  hash (MaxItemCount _) = 5
  hash (AddItems _) = 6
  hash (RemoveItems _) = 7
  hash (RemoveItemButton _) = 8
  hash (EditItems _) = 9
  hash (DuplicateItemsAllowed _) = 10
  hash (Delimiter _) = 11
  hash (Paste _) = 12
  hash (SearchEnabled _) = 13
  hash (SearchChoices _) = 14
  hash (SearchFields _) = 15
  hash (SearchFloor _) = 16
  hash (SearchResultLimit _) = 17
  hash (Position _) = 18
  hash (ResetScrollPosition _) = 19
  hash (AddItemFilter _) = 20
  hash (ShouldSort _) = 21
  hash (ShouldSortItems _) = 22
  hash (Sorter _) = 23
  hash (Placeholder _) = 24
  hash (PlaceholderValue _) = 25
  hash (SearchPlaceholderValue _) = 26
  hash (PrependValue _) = 27
  hash (AppendValue _) = 28
  hash (RenderSelectedChoices _) = 29
  hash (LoadingText _) = 30
  hash (NoResultsText _) = 31
  hash (NoChoicesText _) = 32
  hash (ItemSelectText _) = 33
  hash (AddItemText _) = 34
  hash (MaxItemText _) = 35
  hash (ValueComparer _) = 36
  -- hash (CallbackOnInit _) = 37
  hash (CallbackOnCreateTemplates _) = 38

instance eqOption :: Eq Option where
  eq (Silent _) (Silent _) = true
  eq (Items _) (Items _) = true
  eq (Choices _) (Choices _) = true
  eq (RenderChoiceLimit _) (RenderChoiceLimit _) = true
  eq (MaxItemCount _) (MaxItemCount _) = true
  eq (AddItems _) (AddItems _) = true
  eq (RemoveItems _) (RemoveItems _) = true
  eq (RemoveItemButton _) (RemoveItemButton _) = true
  eq (EditItems _) (EditItems _) = true
  eq (DuplicateItemsAllowed _) (DuplicateItemsAllowed _) = true
  eq (Delimiter _) (Delimiter _) = true
  eq (Paste _) (Paste _) = true
  eq (SearchEnabled _) (SearchEnabled _) = true
  eq (SearchChoices _) (SearchChoices _) = true
  eq (SearchFields _) (SearchFields _) = true
  eq (SearchFloor _) (SearchFloor _) = true
  eq (SearchResultLimit _) (SearchResultLimit _) = true
  eq (Position _) (Position _) = true
  eq (ResetScrollPosition _) (ResetScrollPosition _) = true
  eq (AddItemFilter _) (AddItemFilter _) = true
  eq (ShouldSort _) (ShouldSort _) = true
  eq (ShouldSortItems _) (ShouldSortItems _) = true
  eq (Sorter _) (Sorter _) = true
  eq (Placeholder _) (Placeholder _) = true
  eq (PlaceholderValue _) (PlaceholderValue _) = true
  eq (SearchPlaceholderValue _) (SearchPlaceholderValue _) = true
  eq (PrependValue _) (PrependValue _) = true
  eq (AppendValue _) (AppendValue _) = true
  eq (RenderSelectedChoices _) (RenderSelectedChoices _) = true
  eq (LoadingText _) (LoadingText _) = true
  eq (NoResultsText _) (NoResultsText _) = true
  eq (NoChoicesText _) (NoChoicesText _) = true
  eq (ItemSelectText _) (ItemSelectText _) = true
  eq (AddItemText _) (AddItemText _) = true
  eq (MaxItemText _) (MaxItemText _) = true
  eq (ValueComparer _) (ValueComparer _) = true
  --eq (CallbackOnInit _) (CallbackOnInit _) = true
  eq (CallbackOnCreateTemplates _) (CallbackOnCreateTemplates _) = true
  eq _ _ = false


unsafeConvertEvent :: forall a b c. (Foreign -> ExceptT c Identity b) -> a -> b
unsafeConvertEvent f e = unsafePartial $ fromRight $ runExcept $ f $ unsafeToForeign e


type AddItemEvent =
  { id :: Int
  , value :: String
  , label :: String
  , customProperties :: Foreign
  , groupValue :: String
  , keyCode :: Int
  }

onAddItem :: forall r i. (AddItemEvent -> Maybe i) -> HP.IProp r i
onAddItem f = HE.handler (EventType "addItem") (\e -> f (unsafeConvertEvent convert e))
  where convert e = do
          id               <- e ! "id" >>= readInt 
          value            <- e ! "value" >>= readString
          label            <- e ! "label" >>= readString
          customProperties <- e ! "customProperties"
          groupValue       <- e ! "groupValue" >>= readString
          keyCode          <- e ! "keyCode" >>= readInt
          pure { id, value, label, customProperties, groupValue, keyCode }


type RemoveItemEvent =
  { id :: Int
  , value :: String
  , label :: String
  , customProperties :: Foreign
  , groupValue :: String
  }

onRemoveItem :: forall r i. (RemoveItemEvent -> Maybe i) -> HP.IProp r i
onRemoveItem f = HE.handler (EventType "removeItem") (\e -> f (unsafeConvertEvent convert e))
  where convert e = do
          id               <- e ! "id" >>= readInt 
          value            <- e ! "value" >>= readString
          label            <- e ! "label" >>= readString
          customProperties <- e ! "customProperties"
          groupValue       <- e ! "groupValue" >>= readString
          pure { id, value, label, customProperties, groupValue }


type HighlightItemEvent =
  { id :: Int
  , value :: String
  , label :: String
  , groupValue :: String
  }

onHighlightItem :: forall r i. (HighlightItemEvent -> Maybe i) -> HP.IProp r i
onHighlightItem f = HE.handler (EventType "highlightItem") (\e -> f (unsafeConvertEvent convert e))
  where convert e = do
          id         <- e ! "id" >>= readInt 
          value      <- e ! "value" >>= readString
          label      <- e ! "label" >>= readString
          groupValue <- e ! "groupValue" >>= readString
          pure { id, value, label, groupValue }


type UnhighlightItemEvent =
  { id :: Int
  , value :: String
  , label :: String
  , groupValue :: String
  }

onUnhighlightItem :: forall r i. (UnhighlightItemEvent -> Maybe i) -> HP.IProp r i
onUnhighlightItem f = HE.handler (EventType "unhighlightItem") (\e -> f (unsafeConvertEvent convert e))
  where convert e = do
          id         <- e ! "id" >>= readInt 
          value      <- e ! "value" >>= readString
          label      <- e ! "label" >>= readString
          groupValue <- e ! "groupValue" >>= readString
          pure { id, value, label, groupValue }


onChoice :: forall r i. (Choice -> Maybe i) -> HP.IProp r i
onChoice f = HE.handler (EventType "choice") (\e -> f (unsafeConvertEvent convert e))
  where convert e = e ! "id" >>= unsafeFromForeign >>= pure


onChange :: forall r i. (String -> Maybe i) -> HP.IProp r i
onChange f = HE.handler (EventType "change") (\e -> f (unsafeConvertEvent convert e))
  where convert e = e ! "id" >>= readString >>= pure


type SearchEvent =
  { value :: String
  , resultCount :: Int
  }

onSearch :: forall r i. (SearchEvent -> Maybe i) -> HP.IProp r i
onSearch f = HE.handler (EventType "search") (\e -> f (unsafeConvertEvent convert e))
  where convert e = do 
          value       <- e ! "value" >>= readString
          resultCount <- e ! "label" >>= readInt
          pure { value, resultCount }


onShowDropdown :: forall r i. Maybe i -> HP.IProp r i
onShowDropdown f = HE.handler (EventType "showDropdown") (\_ -> f)


onHideDropdown :: forall r i. Maybe i -> HP.IProp r i
onHideDropdown f = HE.handler (EventType "hideDropdown") (\_ -> f)


onHighlightChoice :: forall r i. (PassedElement -> Maybe i) -> HP.IProp r i
onHighlightChoice f = HE.handler (EventType "highlightChoice") (\e -> f (unsafeConvertEvent convert e))
  where convert e = e ! "id" >>= unsafeFromForeign >>= pure
