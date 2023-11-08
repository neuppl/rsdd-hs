{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.RSDD
  ( varOrderLinear,
    -- , cnfFromDimacs
    roBddBuilderAllTable,
    compileCnf,
    modelCount,
    roBddBuilderDefaultOrder,
    newVar,
    newBddPtr,
    ite,
    bddAnd,
    bddOr,
    bddNeg,
    isTrue,
    isFalse,
    isConst,
    ptrTrue,
    ptrFalse,
    bddEq,
    topvar,
    low,
    high,
    newWmc,
    bddWmc,
    setWeight,
    varWeight,
    printBdd,
    BddBuilder,
    VarOrder,
    Cnf,
    BddPtr,
    VarLabel,
  )
where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Natural
import System.IO.Unsafe

-- dummy rust data types
data RawRsddBddBuilder

data RawVarOrder

data RawCnf

data RawBddPtr

newtype BddBuilder = BddBuilder (Ptr RawRsddBddBuilder)

newtype VarOrder = VarOrder (Ptr RawVarOrder)

newtype Cnf = Cnf (Ptr RawCnf)

newtype BddPtr = BddPtr (Ptr RawBddPtr)

newtype VarLabel = VarLabel Natural

-- Call to the Rust function 'var_order_linear' which returns a pointer to a VarOrder structure
foreign import ccall unsafe "var_order_linear"
  c_var_order_linear ::
    CSize -> IO VarOrder

-- return a variable order for a given number of variables
varOrderLinear :: Natural -> VarOrder
varOrderLinear numVars = unsafePerformIO $ c_var_order_linear (fromIntegral numVars)

-- -- Call to the Rust function 'cnf_from_dimacs' which takes a C string and returns a pointer to a Cnf structure
-- foreign import ccall unsafe "cnf_from_dimacs" c_cnf_from_dimacs
--     :: Ptr CChar -> IO Cnf

foreign import ccall unsafe "robdd_builder_all_table"
  roBddBuilderAllTable ::
    VarOrder -> IO BddBuilder

foreign import ccall unsafe "robdd_builder_compile_cnf"
  compileCnf ::
    BddBuilder -> Cnf -> IO BddPtr

foreign import ccall unsafe "robdd_model_count"
  c_robdd_model_count ::
    BddBuilder -> BddPtr -> IO Word64

modelCount :: BddBuilder -> BddPtr -> IO Natural
modelCount mgr ptr = fromIntegral <$> c_robdd_model_count mgr ptr

-- Creates a new BDD builder with a default variable order
foreign import ccall unsafe "mk_bdd_manager_default_order"
  c_mk_bdd_manager_default_order ::
    Word64 -> IO BddBuilder

roBddBuilderDefaultOrder :: Natural -> IO BddBuilder
roBddBuilderDefaultOrder n = c_mk_bdd_manager_default_order (fromIntegral n)

foreign import ccall unsafe "bdd_new_label"
  c_bdd_new_label :: BddBuilder -> IO Word64

foreign import ccall unsafe "bdd_var"
  c_bdd_var :: BddBuilder -> Word64 -> Bool -> IO BddPtr

newVar :: BddBuilder -> Bool -> IO (VarLabel, BddPtr)
newVar mgr pol = do
  lbl <- c_bdd_new_label mgr
  ptr <- c_bdd_var mgr lbl pol
  pure ((VarLabel . fromIntegral) lbl, ptr)

newBddPtr :: BddBuilder -> Bool -> IO BddPtr
newBddPtr mgr pol = snd <$> newVar mgr pol

-- BDD operations: If-Then-Else
foreign import ccall unsafe "bdd_ite"
  ite ::
    BddBuilder -> BddPtr -> BddPtr -> BddPtr -> IO BddPtr

-- BDD operations: And
foreign import ccall unsafe "bdd_and"
  bddAnd ::
    BddBuilder -> BddPtr -> BddPtr -> IO BddPtr

-- BDD operations: Or
foreign import ccall unsafe "bdd_or"
  bddOr ::
    BddBuilder -> BddPtr -> BddPtr -> IO BddPtr

-- BDD operations: Negate
foreign import ccall unsafe "bdd_negate"
  bddNeg ::
    BddBuilder -> BddPtr -> IO BddPtr

-- checks if the BddPtr is a constant and is PtrTrue
foreign import ccall unsafe "bdd_is_true"
  isTrue :: BddPtr -> Bool

-- checks if the BddPtr is a constant and is PtrFalse
foreign import ccall unsafe "bdd_is_false"
  isFalse :: BddPtr -> Bool

-- checks if the BddPtr is a constant (meaning either PtrTrue or PtrFalse)
foreign import ccall unsafe "bdd_is_const"
  isConst :: BddPtr -> Bool

-- Create constant BDD nodes of True
foreign import ccall unsafe "bdd_true"
  ptrTrue :: BddBuilder -> BddPtr

-- Create constant BDD nodes of False
foreign import ccall unsafe "bdd_false"
  ptrFalse :: BddBuilder -> BddPtr

-- Compare two BDD nodes for equality
foreign import ccall unsafe "bdd_eq"
  bddEq :: BddBuilder -> BddPtr -> BddPtr -> Bool

-- Get the top variable of a BDD node
foreign import ccall unsafe "bdd_topvar"
  c_bdd_topvar ::
    BddPtr -> IO Word64

topvar :: BddPtr -> VarLabel
topvar ptr = unsafePerformIO $ VarLabel . fromIntegral <$> c_bdd_topvar ptr

-- Get the low edge of a BDD node
foreign import ccall unsafe "bdd_low"
  low :: BddPtr -> BddPtr

-- Get the high edge of a BDD node
foreign import ccall unsafe "bdd_high"
  high :: BddPtr -> BddPtr

data RawRsddWmcParamsR

newtype WmcParams = WmcParams (Ptr RawRsddWmcParamsR)

foreign import ccall unsafe "new_wmc_params_f64"
  newWmc :: WmcParams

-- newWmc :: WmcParams
-- newWmc = unsafePerformIO . newWmcIO

foreign import ccall unsafe "bdd_wmc"
  bddWmc :: BddPtr -> WmcParams -> Double

foreign import ccall unsafe "wmc_param_f64_set_weight"
  c_wmc_param_f64_set_weight :: WmcParams -> Word64 -> Double -> Double -> IO ()

setWeight :: WmcParams -> VarLabel -> Double -> Double -> IO ()
setWeight wmc (VarLabel n) = c_wmc_param_f64_set_weight wmc (fromIntegral n)

data RawRsddWmcWeightR

foreign import ccall unsafe "wmc_param_f64_var_weight"
  c_wmc_param_f64_var_weight :: WmcParams -> Word64 -> IO (Ptr RawRsddWmcWeightR)

foreign import ccall unsafe "weight_f64_lo"
  c_weight_f64_lo :: Ptr RawRsddWmcWeightR -> IO Double

foreign import ccall unsafe "weight_f64_hi"
  c_weight_f64_hi :: Ptr RawRsddWmcWeightR -> IO Double

varWeight :: WmcParams -> VarLabel -> (Double, Double)
varWeight wmc (VarLabel v) = unsafePerformIO $
  c_wmc_param_f64_var_weight wmc (fromIntegral v) >>= \n ->
    (,)
      <$> c_weight_f64_lo n
      <*> c_weight_f64_hi n

foreign import ccall unsafe "print_bdd"
  c_print_bdd :: BddPtr -> IO CString

printBdd :: BddPtr -> IO String
printBdd ptr = c_print_bdd ptr >>= peekCString

instance Show BddPtr where
  show = unsafePerformIO . printBdd

-------------------------

-- * Haskell-friendly wrappers

-- -- Converts a Haskell String to a C string, calls the Rust function, and returns a pointer to Cnf
-- cnfFromDimacs :: String -> IO Cnf
-- cnfFromDimacs dimacsStr = withCString dimacsStr c_cnf_from_dimacs
