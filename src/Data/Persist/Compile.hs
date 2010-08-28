module Data.Persist.Compile (compile) where

import Data.Persist.AST
import Language.Haskell.Exts.Syntax
import Data.Either (partitionEithers)


compile :: [Either Decl Relationship] -> Module
compile input = Module noLoc (ModuleName "Model") pragmas Nothing Nothing imports (compileDecls input)
 where imports     = map mkImport ["Data.Persist.Interface", "Generics.Regular"]
       pragmas     = [LanguagePragma noLoc $ map Ident ["TemplateHaskell", "EmptyDataDecls", "TypeFamilies"]]
       mkImport nm = ImportDecl noLoc (ModuleName nm) False False Nothing Nothing Nothing


compileDecls :: [Either Decl Relationship] -> [Decl]
compileDecls input = let (decls, relationships) = partitionEithers input
                         relationshipsBothDirections = relationships ++ (map reverseRelationship relationships)
                         dbClass = UnQual (Ident "Persistent")
                in concat [ decls
                          , concatMap derivingRegular decls
                          , concatMap (compileRelationship decls) relationships
                          , concatMap (createMethod dbClass relationshipsBothDirections) decls
                          , createSchema dbClass decls relationships
                          ]

compileRelationship :: [Decl] -> Relationship -> [Decl]
compileRelationship _ r = [ TypeSig noLoc [funName] (relType `TyApp` from `TyApp` to)
                          , FunBind [Match noLoc funName [] Nothing rhs (BDecls [])]
                          ]
 where funName = Ident $ relName r
       rhs     = UnGuardedRhs $ Con (UnQual (Ident "Relation")) `App` (Lit $ String $ relName r)
       from    = TyCon (UnQual (Ident (relFromName r)))
       to      = TyCon (UnQual (Ident (relToName r)))



createMethod :: QName -> [Relationship] -> Decl -> [Decl]
createMethod dbClass rs  d = 
  [ TypeSig noLoc [funName] (TyForall Nothing ctx $ TyFun (typ d) (f (TyApp monad (TyApp refType (typ d)))))
  , FunBind [Match noLoc funName ((PVar (Ident "value")):(map (PVar . Ident . snd) relArgs)) Nothing (UnGuardedRhs (rhs relArgs) ) (BDecls [])]
  ]
  where ctx     = [ClassA dbClass [monad]]
        monad   = TyVar (Ident "db")
        funName      = Ident $ "create" ++ datatypeName 

        relArgs = zip rels (map relVarName [1..])
        relVarName x = "x" ++ show x

        datatypeName = name d
        rels    = involvedRelationships datatypeName rs

        f = relationshipsToFun rels

rhs relArgs = Do $ concat 
  [ [ Generator noLoc (PVar (Ident "i")) (App (var "create_") (var "value")) ]
  , concatMap relCreate relArgs
  , [ Qualifier $ App (var "return") (var "i") ]
  ]
  where relCreate :: (Relationship, String) -> [Stmt]
        relCreate (r,s) = [ addRelation r s ]
        addRelation r s | reversed r == False = Qualifier $ App (App (var "addRelation") (var "i")) (var s)
                        | otherwise           = Qualifier $ var "addRelation" `App` var s `App` var "i" `App` var (relName r)
                        
createSchema :: QName -> [Decl] -> [Relationship] -> [Decl]
createSchema dbClass decls rels = 
  [ TypeSig noLoc [funName] (TyForall Nothing ctx $ monad `TyApp` unit_tycon)
  , FunBind [Match noLoc funName [] Nothing (UnGuardedRhs (schemaRhs decls rels)) (BDecls []) ] 
  ]
  where ctx = [ClassA dbClass [monad]]
        monad = TyVar (Ident "db")
        schemaRhs decls rels = Do (map entSchema decls ++ map relSchema rels)
        funName = Ident "createSchema"
        entSchema ent = Qualifier $ App (var "createSchemaEntity_") (ExpTypeSig noLoc (var "undefined") (TyCon (UnQual (Ident $ name ent))))
        relSchema rel = Qualifier $ App (var "createSchemaRelationship_") (var $ relName rel)

var :: String -> Exp
var = Var . UnQual . Ident

relationshipsToFun :: [Relationship] -> (Type -> Type)
relationshipsToFun []     = id
relationshipsToFun (x:xs) = TyFun (TyApp refType $ TyCon (UnQual (Ident (relToName x)))) . relationshipsToFun xs

derivingRegular :: Decl -> [Decl]
derivingRegular x = [ SpliceDecl noLoc $ SpliceExp $ ParenSplice $ var "deriveAll" `App` TypQuote typeName `App` (Lit $ String pfName)
                    , TypeInsDecl noLoc (TyCon pFType `TyApp` TyCon typeName) (TyCon $ UnQual $ Ident pfName)
                    ]
 where nm = name x
       pfName = "PF" ++ nm
       typeName = UnQual $ Ident nm

involvedRelationships :: String -> [Relationship] -> [Relationship]
involvedRelationships d = filter (\r -> relFromName r == d && isToOne r)

typ :: Decl -> Type
typ (DataDecl _ _ _ nm  _ _ _) = TyCon (UnQual nm)
typ _ = error "Compile.typ"

pFType :: QName
pFType = (UnQual (Ident "PF"))

relType :: Type
relType = TyCon (UnQual (Ident "Relation"))

refType :: Type
refType = TyCon (UnQual (Ident "Ref"))

name :: Decl -> String
name (DataDecl _ _ _ (Ident nm)  _ _ _) = nm
name s = error $ "Compile.name" ++ show s

noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0
