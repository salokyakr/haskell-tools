{-# LANGUAGE ScopedTypeVariables #-}

-- | Transform a syntax tree with ranges to a syntax tree that has range templates. Cuts the ranges of children
-- from the ranges of their parents and replaces it with placeholders.
module Language.Haskell.Tools.PrettyPrint.Prepare.RangeToRangeTemplate (cutUpRanges, fixRanges, BreakUpProblem(..)) where

import Language.Haskell.Tools.AST

import Control.Exception (Exception, throw)
import Control.Monad.State
import Control.Reference ((^.))
import Data.List
import Data.Maybe (Maybe(..), mapMaybe)

import FastString as GHC (unpackFS, mkFastString)
import SrcLoc
import Debug.Trace (trace)
import Data.Data (toConstr)
import Language.Haskell.Tools.PrettyPrint.Prepare.RangeTemplate

-- | Creates a source template from the ranges and the input file.
-- All source ranges must be good ranges.
cutUpRanges :: forall node dom . SourceInfoTraversal node
                 => Ann node dom NormRangeStage
                 -> Ann node dom RngTemplateStage
cutUpRanges n = evalState (cutUpRanges' n) [[],[]]
  where cutUpRanges' :: Ann node dom NormRangeStage -> State [[SrcSpan]] (Ann node dom RngTemplateStage)
        cutUpRanges' = sourceInfoTraverseUp (SourceInfoTrf (trf cutOutElemSpan) (trf cutOutElemList) (trf cutOutElemOpt)) desc asc

        -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail

        -- combine the current node with its children, and add it to the list of current nodes
        trf :: HasRange (x RngTemplateStage)
            => ([SrcSpan] -> x NormRangeStage -> x RngTemplateStage) -> x NormRangeStage -> State [[SrcSpan]] (x RngTemplateStage)
        trf f ni = do stack <- get
                      case stack of
                        (below1 : top : xs) -> do
                          let below = filter isGoodSrcSpan below1
                          let res = f (trace ("Reached cutUpRanges 1" ++ show below) below) ni
                          let ranges = getRange res
                          if (isGoodSrcSpan ranges) then put ([] : (top ++ [ ranges ]) : xs) else  put ([] : (top : xs))
                          return res
                        _ -> trfProblem "RangeToRangeTemplate.cutUpRanges.trf: stack is not right"

-- | Cuts out a list of source ranges from a given range
cutOutElemSpan :: [SrcSpan] -> SpanInfo NormRangeStage -> SpanInfo RngTemplateStage
cutOutElemSpan sps (NormNodeInfo (RealSrcSpan sp))
  = RangeTemplateNode sp $ foldl (breakFirstHit False) (foldl (breakFirstHit False) [RangeElem sp] loc) span
  where (loc,span) = partition (\sp -> srcSpanStart sp == srcSpanEnd sp) sps
        breakFirstHit opt (elem:rest) sp
          = case breakUpRangeElem elem sp of
             -- only continue if the correct place for the child range is not found
              Just pieces -> trace "Reached cutOutElemSpan 1"  $ pieces ++ rest
              Nothing -> trace ("Reached cutOutElemSpan 2" ++ show elem) $ elem : (breakFirstHit False) rest sp
        breakFirstHit opt sps (RealSrcSpan inner) = [RangeElem inner] --trace "Reached cutOutElemSpan 3" $ throw $ BreakUpProblem sp inner sps
        breakFirstHit opt [] inner = trace ("Reached cutOutElemSpan 3" ++ show sps) $ throw $ BreakUpProblem sp inner sps
cutOutElemSpan _ (NormNodeInfo (UnhelpfulSpan {}))
  = RangeTemplateNode (mkRealSrcSpan (mkRealSrcLoc (mkFastString "") 100 100) (mkRealSrcLoc (mkFastString "") 100 100)) [RangeChildElem]

data BreakUpProblem = BreakUpProblem { bupOuter :: RealSrcSpan
                                     , bupInner :: SrcSpan
                                     , bupSiblings :: [SrcSpan]
                                     }

instance Show BreakUpProblem where
 show (BreakUpProblem _ (RealSrcSpan inner) _)
   = unpackFS (srcSpanFile inner) ++ ": didn't find correct place for AST element at " ++ shortShowSpan (RealSrcSpan inner)
 show (BreakUpProblem outer _ _)
   = unpackFS (srcSpanFile outer) ++ ": didn't find correct place for AST element in " ++ shortShowSpan (RealSrcSpan outer)

instance Exception BreakUpProblem

cutOutElemList :: [SrcSpan] -> ListInfo NormRangeStage -> ListInfo RngTemplateStage
cutOutElemList sps (NormListInfo bef aft sep indented sp)
  = let range = foldl1 combineSrcSpans $ trace ("Reached cutOutElemList 1" ++ (show (sp : sps))) $ sp : sps
        wholeRange = getFakeRange range
     in RangeTemplateList wholeRange bef aft sep indented (getSeparators wholeRange sps)

getFakeRange (RealSrcSpan sp) = sp
getFakeRange _ = mkRealSrcSpan (mkRealSrcLoc (mkFastString "") 100 100) (mkRealSrcLoc (mkFastString "") 100 100)
-- | Cuts out all elements from a list, the rest is the list of separators
getSeparators :: RealSrcSpan -> [SrcSpan] -> [RealSrcSpan]
getSeparators sp infos@(_:_:_)
  = mapMaybe getRangeElemSpan (cutOutElemSpan infos (NormNodeInfo (RealSrcSpan sp)) ^. rngTemplateNodeElems)
-- at least two elements needed or there can be no separators
getSeparators _ _ = []

cutOutElemOpt :: [SrcSpan] -> OptionalInfo NormRangeStage -> OptionalInfo RngTemplateStage
cutOutElemOpt sps (NormOptInfo bef aft sp)
  = let range = foldl1 combineSrcSpans $ sp : sps
     in RangeTemplateOpt (getFakeRange range) bef aft

-- | Breaks the given template element into possibly 2 or 3 parts by cutting out the given part
-- if it is inside the range of the template element. Returns Nothing if the second argument is not inside.
breakUpRangeElem :: RangeTemplateElem -> SrcSpan -> Maybe [RangeTemplateElem]
breakUpRangeElem (RangeElem outer) (RealSrcSpan inner)
  | outer `containsSpan` inner
  = Just $ (if (realSrcSpanStart outer) < (realSrcSpanStart inner)
              then [ RangeElem (mkRealSrcSpan (realSrcSpanStart outer) (realSrcSpanStart inner)) ]
              else []) ++
           [ RangeChildElem ] ++
           (if (realSrcSpanEnd inner) < (realSrcSpanEnd outer)
              then [ RangeElem (mkRealSrcSpan (realSrcSpanEnd inner) (realSrcSpanEnd outer)) ]
              else [])
breakUpRangeElem _ _ = Nothing


-- | Modifies ranges to contain their children
fixRanges :: SourceInfoTraversal node
          => Ann node dom RangeStage
          -> Ann node dom NormRangeStage
fixRanges node = evalState (sourceInfoTraverseUp (SourceInfoTrf (trf expandToContain) (trf expandListToContain) (trf expandOptToContain)) desc asc node) [[],[]]
  where -- keep the stack to contain the children elements on the place of the parent element
        desc = modify ([]:)
        asc  = modify tail


        trf :: HasRange (x NormRangeStage)
            => ([SrcSpan] -> x RangeStage -> x NormRangeStage) -> x RangeStage -> State [[SrcSpan]] (x NormRangeStage)
        trf f ni = do stack <- get
                      case stack of
                        (below : top : xs) -> do
                          let res = f (trace ("Reached fixRanges 1" ++ show below ) below) ni
                              resRange = trace ("Reached fixRanges 2" ++ (show $ getRange res)) $ getRange res
                              endOfSiblings = srcSpanEnd (collectSpanRanges (srcSpanStart resRange) top)
                              correctedRange = if endOfSiblings > srcSpanStart resRange
                                                 then mkSrcSpan endOfSiblings (max endOfSiblings (srcSpanEnd resRange))
                                                 else resRange
                          if (isGoodSrcSpan correctedRange) then do
                            (put ([] : (top ++ [ correctedRange ]) : xs))
                            return $ setRange correctedRange res
                            else do
                              (put ([] : (top : xs)))
                              return $ setRange (mkSrcSpan endOfSiblings (max endOfSiblings (srcSpanEnd resRange))) res
                        _ -> trfProblem "RangeToRangeTemplate.fixRanges.trf: stack is not right"

-- | Expand a simple node to contain its children
expandToContain :: [SrcSpan] -> SpanInfo RangeStage -> SpanInfo NormRangeStage
expandToContain cont (NodeSpan sp)
  =  NormNodeInfo (trace ("Reached expandToContain 2") $ checkSpans cont $ trace ("Reached expandToContain 1") $ foldl1 combineSrcSpans $ sp : cont)

expandListToContain :: [SrcSpan] -> ListInfo RangeStage -> ListInfo NormRangeStage
expandListToContain cont (ListPos bef aft def ind sp)
  =   NormListInfo bef aft def ind (trace ("Reached expandListToContain 2") $ checkSpans cont $ trace ("Reached expandListToContain 1") $ collectSpanRanges sp cont)

expandOptToContain :: [SrcSpan] -> OptionalInfo RangeStage -> OptionalInfo NormRangeStage
expandOptToContain cont (OptionalPos bef aft sp)
  =  NormOptInfo bef aft (trace ("Reached expandOptToContain 2") $ checkSpans cont $ trace ("Reached expandOptToContain 1") $ collectSpanRanges sp cont)

collectSpanRanges :: SrcLoc -> [SrcSpan] -> SrcSpan
collectSpanRanges loc@(RealSrcLoc _) [] = srcLocSpan loc
collectSpanRanges _ ls = foldl combineSrcSpans noSrcSpan ls

-- | Checks the contained source ranges to detect the convertion problems where we can see their location.
checkSpans :: [SrcSpan] -> SrcSpan -> SrcSpan
checkSpans spans res
  = if (trace ("Reached checkSpans 2" ++ (show $ any (not . isGoodSrcSpan) spans) ++ (show $ shortShowSpan <$> spans)) $ any (not . isGoodSrcSpan) spans) && (trace ("Reached checkSpans 1" ++ show (isGoodSrcSpan res) ++ shortShowSpan res) $ isGoodSrcSpan res)
      then trfProblem $ "Wrong src spans in " ++ show res
      else res
