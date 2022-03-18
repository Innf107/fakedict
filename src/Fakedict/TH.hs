{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Fakedict.TH (makeDict) where

import Fakedict.Internal

import Language.Haskell.TH

makeDict :: Name -> Q [Dec]
makeDict cname = do
    reify cname >>= \case
        ClassI cdec is -> case cdec of
            ClassD cxt cname tvs _ meths -> do 
                (<>) [
                        dataCon [] dname tvs Nothing [ForallC (map (addSpecificity SpecifiedSpec) tvs) cxt $ RecGadtC [dname] (map methodToVarBangType meths) appliedFakeType] []
                    ]
                <$> [d|instance FakeDictFor $(pure appliedClassType) $(pure appliedFakeType)|]
                where
                    dataCon = case (meths, cxt) of
                        ([_], []) -> \cxt name tvs k [cs] ds -> NewtypeD cxt name tvs k cs ds 
                        _ -> DataD
                    dname = mkName $ nameBase cname <> "Dict"
                    
                    methodToVarBangType (SigD n t) = (mkName ("_" <> nameBase n), Bang SourceNoUnpack NoSourceStrictness, t)
                    
                    appliedFakeType  = foldl AppT (ConT dname) (map (VarT . tyVarName) tvs)
                    appliedClassType = foldl AppT (ConT cname) (map (VarT . tyVarName) tvs)

                    addSpecificity s (PlainTV n _) = PlainTV n s
                    addSpecificity s (KindedTV n _ k) = KindedTV n s k

                    tyVarName (PlainTV n _) = n
                    tyVarName (KindedTV n _ _) = n
            _ -> fail $ "Not a class: " <> show cname
        _ -> fail $ "Not a class: " <> show cname

