{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Voobly.TH where


import RIO
import Language.Haskell.TH
import Data.IxSet.Typed

makeSimpleIxSet :: String -> Name -> [Name] -> Q [Dec]
makeSimpleIxSet setName typeName accessorNames = do
  accessorInfos <- mapM reify accessorNames
  let resultTypes = map (\ (VarI _ t _ ) -> getResultType t) accessorInfos
      resultTypesForIndex = map getResultTypeForIndex resultTypes
      indexList   = return $
                      foldr (\ x r -> PromotedConsT `AppT` x `AppT` r)
                            PromotedNilT
                            resultTypesForIndex
  synDecl <- tySynD (mkName setName) [] [t| IxSet $(indexList) $(conT typeName) |]
  --let ixFuns = map (\ n -> [| ixFun (\ x -> [$(varE n) x]) |]) accessorNames
  let ixFuns = map (getIxFunDef) (zip accessorNames resultTypes)

  clsDecl <- [d| instance Indexable $(indexList) $(conT typeName) where
                   indices = $(appsE ([| ixList |] : ixFuns))
               |]
  return (synDecl : clsDecl)
  where
    getIxFunDef :: (Name, Type) -> Q Exp
    getIxFunDef (n, AppT ListT _) = [| ixFun (\ x -> $(varE n) x) |]
    getIxFunDef (n, _) = [| ixFun (\ x -> [$(varE n) x]) |]
    getResultType :: Type -> Type
    getResultType (ForallT _ _ t)          = getResultType t
    getResultType (AppT (AppT ArrowT _) t) = getResultType t
    getResultType t                        = t
    getResultTypeForIndex :: Type -> Type
    getResultTypeForIndex (AppT ListT t) = t
    getResultTypeForIndex t = t