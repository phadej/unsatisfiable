{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
-- | A type-checker plugin for 'Unsatisfiable.Class.Unsatisfiable' type class.
module Unsatisfiable.Plugin (
    plugin,
) where

import Control.Monad          (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor           ((<&>))
import Data.Maybe             (listToMaybe)

import qualified GHC
import qualified GHC.Builtin.Types       as GHC
import qualified GHC.Core                as GHC
import qualified GHC.Core.Class          as GHC
import qualified GHC.Core.Make           as GHC
import qualified GHC.Core.TyCon          as GHC
import qualified GHC.Core.Type           as GHC
import qualified GHC.Driver.Session      as GHC
import qualified GHC.Tc.Types            as GHC
import qualified GHC.Tc.Types.Constraint as GHC
import qualified GHC.Tc.Types.Evidence   as GHC
import qualified GHC.Tc.Utils.Monad      as GHC
import qualified GHC.Types.Id            as GHC
import qualified GHC.Types.Literal       as GHC
import qualified GHC.Types.Name          as GHC
import qualified GHC.Types.Var           as GHC
import qualified GHC.Utils.Outputable    as GHC

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Plugins   as Plugins
import qualified GHC.Tc.Plugin as Plugins
#else
import qualified GhcPlugins as Plugins
import qualified TcPluginM  as Plugins
#endif

-------------------------------------------------------------------------------
-- Plugin
-------------------------------------------------------------------------------

-- | To use this plugin add
--
-- @
-- \{-# OPTIONS_GHC -fplugin=Unsatisfiable #-}
-- @
--
-- to your source file.
--
-- This plugin does two things:
--
-- Firstly, when there is /wanted/ 'Unsatisfiable.Class.Unsatisfiable' constraint,
-- we pretty-pring its message.
-- Unfortunately the actual @No instance for (Unsatisfiable msg)@
-- error is also printed, as we don't solve it, except when
-- @-fdefer-type-errors@ is enabled.
-- In that case 'Unsatifiable.Class.unsatisfiable' will throw an error with
-- the rendered message.
--
-- Secondly, when 'Unsatisfiable.Class.Unsatisfiable' constraint is given,
-- all other constraints are solved automatically using 'unsatisfiable'
-- as the evidence. This is useful 
--
-- @
-- class C a => D a
-- instance Unsatisfiable Msg => D Int  -- Note absence of C Int in the context
-- @
--
-- The motivation here is that if we use 'Unsatisifable.Class.Unsatisfiable' to
-- ban an instance with a nice error message, we don't want to get extra error
-- messages arising from failure to solve the superclass constraints, which we
-- get if we are obliged to use
--
-- @
-- instance (Unsatisfiable Msg, C Int) => D Int
-- @
--
plugin :: Plugins.Plugin
plugin = Plugins.defaultPlugin
    { Plugins.tcPlugin            = \_args -> Just tcPlugin
    , Plugins.pluginRecompile     = Plugins.purePlugin
    }

tcPlugin :: GHC.TcPlugin
tcPlugin = GHC.TcPlugin
    { GHC.tcPluginInit  = tcPluginInit
    , GHC.tcPluginSolve = tcPluginSolve
    , GHC.tcPluginStop  = const (return ())
    }

-------------------------------------------------------------------------------
-- Ctx
-------------------------------------------------------------------------------

data PluginCtx = PluginCtx
    { unsatisfiableCls  :: GHC.Class
    , unsatisfiableExpr :: GHC.Var
    }

-------------------------------------------------------------------------------
-- Plugin functions
-------------------------------------------------------------------------------

tcPluginInit :: GHC.TcPluginM PluginCtx
tcPluginInit = do
    debug $ Plugins.tcPluginIO $ putStrLn "INITIALIZING UNSATISFIABLE PLUGIN"

    dflags <- GHC.unsafeTcPluginTcM GHC.getDynFlags

    let findModule :: GHC.ModuleName -> Plugins.TcPluginM GHC.Module
        findModule m = do
            im <- Plugins.findImportedModule m Nothing
            case im of
                Plugins.Found _ md -> return md
                _              -> do
                    Plugins.tcPluginIO $ putError dflags GHC.noSrcSpan  $
                        GHC.text "Cannot find module" GHC.<+> GHC.ppr m
                    fail "panic!"

    md <- findModule unsatisfiableClassMN
    unsatisfiableCls <- do
        name <- Plugins.lookupOrig md (GHC.mkTcOcc "Unsatisfiable")
        Plugins.tcLookupClass name

    unsatisfiableExpr <- do
        name <- Plugins.lookupOrig md (GHC.mkVarOcc "unsatisfiable_")
        Plugins.tcLookupId name

    debug $ do
        Plugins.tcPluginIO $ putStrLn $ GHC.showSDoc dflags $ GHC.ppr unsatisfiableCls
        Plugins.tcPluginIO $ putStrLn $ GHC.showSDoc dflags $ GHC.ppr unsatisfiableExpr
        Plugins.tcPluginIO $ putStrLn $ GHC.showSDoc dflags $ GHC.ppr $ GHC.idScaledType unsatisfiableExpr

    return PluginCtx {..}

tcPluginSolve :: PluginCtx -> GHC.TcPluginSolver
tcPluginSolve ctx givens _deriveds wanteds = do
    -- acquire context
    dflags       <- Plugins.unsafeTcPluginTcM GHC.getDynFlags
    let defer_errors = GHC.gopt GHC.Opt_DeferTypeErrors dflags

    -- let us see if there is Unsatisfiable constraint in givens.
    let givenUnsatisfiable :: Maybe (GHC.Ct, GHC.Type)
        givenUnsatisfiable = listToMaybe
            [ (ct, ty)
            | ct <- givens
            , Just ty <- return (isUnsatisfiable ctx ct)
            ]

    debug $ Plugins.tcPluginIO $ putStrLn $ GHC.showSDoc dflags $ GHC.text "givenUnsatisfiable =" GHC.<+> GHC.ppr givenUnsatisfiable

    case givenUnsatisfiable of
        -- there is given unsatisfiable.
        -- Jackpot, we can conjure everything.
        --
        -- we make evidence using (unexported) unsafistiable_
        Just (ctUnsatisfiable, msg) -> do
            let expr :: GHC.Type -> GHC.EvTerm
                expr = evUnsatisfiable ctx msg ctUnsatisfiable

            let solved :: [(GHC.EvTerm, GHC.Ct)]
                solved =
                    [ (expr (GHC.ctev_pred (GHC.ctEvidence ct)), ct)
                    | ct <- wanteds
                    ]


            debug $ do
                forM_ wanteds $ \ct -> Plugins.tcPluginIO $ do
                    putStrLn $ GHC.showSDoc dflags $ GHC.ppr ct
                    putStrLn $ GHC.showSDoc dflags $ GHC.ppr $ GHC.ctev_pred $ GHC.ctEvidence ct

                forM_ solved $ \(t, _) -> Plugins.tcPluginIO $ do
                    putStrLn $ GHC.showSDoc dflags $ GHC.ppr t

            -- return solveds
            return $ GHC.TcPluginOk solved []

        -- ... otherwise
        --
        -- we traverse wanteds, and if there is Unsafistifiable
        -- we pretty-print its message
        Nothing
            | defer_errors -> do
                let solved' :: [(Plugins.TcPluginM GHC.EvTerm, GHC.Ct)]
                    solved' =
                        [ (evDeferredUnsatisfiable ctx dflags msg (GHC.ctLocSpan $ GHC.ctLoc ct), ct)
                        | ct <- wanteds
                        , Just msg <- return (isUnsatisfiable ctx ct)
                        ]

                solved <- (traverse . _1) id solved'

                debug $ do
                    forM_ solved $ \(t, _) -> Plugins.tcPluginIO $ do
                        putStrLn $ GHC.showSDoc dflags $ GHC.ppr t

                -- return solveds
                return $ GHC.TcPluginOk solved []

            | otherwise -> do
                forM_ wanteds $ \ct -> forM_ (isUnsatisfiable ctx ct) $ \ty -> Plugins.tcPluginIO $ do
                    putError dflags (GHC.RealSrcSpan (GHC.ctLocSpan $ GHC.ctLoc ct) Nothing) $
                        GHC.text "Unsatisfiable:" GHC.$$
                        GHC.pprUserTypeErrorTy ty

                -- in this case we don't solve anything.
                return $ GHC.TcPluginOk [] []

evUnsatisfiable :: PluginCtx -> GHC.Type -> GHC.Ct -> GHC.Type -> GHC.EvTerm
evUnsatisfiable ctx msg ct ty =
    -- same trick as evDelayedError
    GHC.EvExpr $ GHC.mkWildCase expr (GHC.unrestricted GHC.unitTy) ty []
  where
    expr = GHC.Var (unsatisfiableExpr ctx)
        `GHC.mkTyApps` [msg]
        `GHC.mkApps` [ GHC.Var (GHC.ctEvId ct) ]
        `GHC.mkTyApps` [GHC.unitTy]

evDeferredUnsatisfiable :: PluginCtx -> GHC.DynFlags -> GHC.Type -> Plugins.RealSrcSpan -> Plugins.TcPluginM GHC.EvTerm
evDeferredUnsatisfiable ctx dflags msg loc = do
    v <- makeTyVar "a" GHC.liftedTypeKind
    return $ GHC.EvExpr $ appDc v
  where
    cls     = unsatisfiableCls ctx
    tyCon   = GHC.classTyCon cls
    dc      = GHC.tyConSingleDataCon tyCon
    appDc v = GHC.mkCoreConApps dc
        [ GHC.Type msg
        , GHC.mkCoreLams [v] (err $ GHC.mkTyVarTy v)
        ]

    err ty  = GHC.Var GHC.tYPE_ERROR_ID
        `GHC.mkTyApps` [GHC.getRuntimeRep ty, ty]
        `GHC.mkApps`   [litMsg]

    litMsg  = GHC.Lit $ GHC.mkLitString $ GHC.showSDoc dflags $
            GHC.text "Unsatisfiable at " GHC.<+> GHC.ppr loc GHC.$$
            GHC.pprUserTypeErrorTy msg GHC.$$
            GHC.text "(deferred unsatisfiable type error)"


-- | Is the constraint a @Unsatisfiable msg@, if so, return @Just msg@.
isUnsatisfiable :: PluginCtx -> GHC.Ct -> Maybe GHC.Type
isUnsatisfiable ctx (GHC.CDictCan _ev cls [ty] _pend_sc)
    | cls == unsatisfiableCls ctx
    = Just ty

isUnsatisfiable _ _ = Nothing

-------------------------------------------------------------------------------
-- Diagnostics
-------------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,0,0)
#define ERR_STYLE
#else
#define ERR_STYLE (GHC.defaultErrStyle dflags)
#endif

debug :: Monad m => m () -> m ()
-- debug x = x
debug _ = return ()

putError :: MonadIO m => GHC.DynFlags -> GHC.SrcSpan -> GHC.SDoc -> m ()
putError dflags l doc =
    liftIO $ GHC.putLogMsg dflags GHC.NoReason GHC.SevError l ERR_STYLE doc

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

unsatisfiableClassMN :: GHC.ModuleName
unsatisfiableClassMN =  GHC.mkModuleName "Unsatisfiable.Class"

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

makeTyVar :: String -> GHC.Kind -> Plugins.TcPluginM GHC.TyVar
makeTyVar n ki = do
    name <- GHC.unsafeTcPluginTcM $ GHC.newName (GHC.mkTyVarOcc n)
    return (GHC.mkTyVar name ki)

_1 :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
_1 f (x, y) = f x <&> \x' -> (x', y)
