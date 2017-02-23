{-# LANGUAGE UnicodeSyntax #-}
module System.Linux.ProcStat ( ProcState (..)
                             , ProcFlag  (..)
                             , ProcInfo  (..)
                             , Pid
                             , procStat
                             ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Word
import Data.Attoparsec.Char8
import Control.Applicative
import System.IO
import Control.Exception (bracket)
import Data.Bits

type Pid = Int

data ProcState = Running
               | Sleeping
               | DiskSleeping
               | Zombie
               | Traced
               | Paging
               deriving Show

data ProcFlag = KSoftIrqD
              | Starting
              | Exiting
              | ExitPidDone
              | VCpu
              | WqWorker
              | ForkNoExec
              | MceProcess
              | SuperPriv
              | DumpCore
              | Signaled
              | MemAlloc
              | UsedMath
              | Freezing
              | NoFreeze
              | Frozen
              | FsTrans
              | KSwapD
              | OomOrigin
              | LessThrottle
              | KThread
              | Randomize
              | SwapWrite
              | SpreadPage
              | SpreadSlab
              | ThreadBound
              | MceEarly
              | MemPolicy
              | MutexTester
              | FreezerSkip
              | FreezerNoSig
              deriving (Show, Enum)

data ProcInfo = ProcInfo { procPid          ∷ Pid
                         , procName         ∷ ByteString
                         , procState        ∷ ProcState
                         , procPPid         ∷ Int
                         , procGId          ∷ Int
                         , procSId          ∷ Int
                         , procTty          ∷ Int
                         , procTtyGid       ∷ Int
                         , procFlags        ∷ [ProcFlag]
                         , procMinFlt       ∷ Word
                         , procCMinFlt      ∷ Word
                         , procMajFlt       ∷ Word
                         , procCMajFlt      ∷ Word
                         , procUTime        ∷ Word
                         , procSTime        ∷ Word
                         , procCUTime       ∷ Int
                         , procCSTime       ∷ Int
                         , procPriority     ∷ Int
                         , procNice         ∷ Int
                         , procNumThreads   ∷ Int
                         , procStartTime    ∷ Word64
                         , procVSize        ∷ Word
                         , procRss          ∷ Int
                         , procRssLim       ∷ Word
                         , procStartCode    ∷ Word
                         , procEndCode      ∷ Word
                         , procStartStack   ∷ Word
                         , procEsp          ∷ Word
                         , procEip          ∷ Word
                         , procSignal       ∷ Word
                         , procBlocked      ∷ Word
                         , procSigIgnore    ∷ Word
                         , procSigCatch     ∷ Word
                         , procWChan        ∷ Word
                         , procNSwap        ∷ Word
                         , procCNSwap       ∷ Word
                         , procExitSignal   ∷ Int
                         , procCpuNum       ∷ Int
                         , procRtPriority   ∷ Word
                         , procPolicy       ∷ Word
                         , procBlkIoTicks   ∷ Word64
                         , procGuestTime    ∷ Word
                         , procCGuestTime   ∷ Int
                         } deriving Show

charToProcState ∷ Char → Maybe ProcState
charToProcState c = case c of
    'R' → Just Running
    'S' → Just Sleeping
    'D' → Just DiskSleeping
    'Z' → Just Zombie
    'T' → Just Traced
    'W' → Just Paging
    _   → Nothing

flagToProcFlags ∷ Word → [ProcFlag]
flagToProcFlags x = foldr step [] flagMap
  where
    flagMap = zip (iterate (*2) 1) [KSoftIrqD .. FreezerNoSig]
    hasFlag ∷ Bits a ⇒ a → a → Bool
    hasFlag a b = a .&. b == b
    step (val, pflag) pflags | x `hasFlag` val = pflag : pflags
                             | otherwise       = pflags

parser ∷ Parser ProcInfo
parser = do
    pid             ← sdc
    name            ← dos $ char '(' *> takeTill (== ')') <* anyChar
    state           ← maybe empty return =<< charToProcState <$> dos anyChar
    ppid            ← sdc
    gid             ← sdc
    sid             ← sdc
    tty             ← sdc
    ttyGid          ← sdc
    flags           ← flagToProcFlags <$> sdc
    minFlt          ← dc
    cMinFlt         ← dc
    majFlt          ← dc
    cMajFlt         ← dc
    uTime           ← dc
    sTime           ← dc
    cuTime          ← sdc
    csTime          ← sdc
    pri             ← sdc
    nice            ← sdc
    nt              ← sdc
    _               ← dos $ char '0'
    st              ← sdc
    vSize           ← dc
    rss             ← dc
    rssLim          ← dc
    startCode       ← dc
    endCode         ← dc
    startStack      ← dc
    esp             ← dc
    eip             ← dc
    signal          ← dc
    blockedS        ← dc
    sigIgnore       ← dc
    sigCatch        ← dc
    wChan           ← dc
    nSwap           ← dc
    cnSwap          ← dc
    exitSignal      ← sdc
    cpuNum          ← sdc
    rtPri           ← dc
    policy          ← dc
    blkIOTicks      ← dc
    guestTime       ← dc
    cGuestTime      ← signed decimal
    return $ ProcInfo pid
                      name
                      state
                      ppid
                      gid
                      sid
                      tty
                      ttyGid
                      flags
                      minFlt
                      cMinFlt
                      majFlt
                      cMajFlt
                      uTime
                      sTime
                      cuTime
                      csTime
                      pri
                      nice
                      nt
                      st
                      vSize
                      rss
                      rssLim
                      startCode
                      endCode
                      startStack
                      esp
                      eip
                      signal
                      blockedS
                      sigIgnore
                      sigCatch
                      wChan
                      nSwap
                      cnSwap
                      exitSignal
                      cpuNum
                      rtPri
                      policy
                      blkIOTicks
                      guestTime
                      cGuestTime
  where
    dos ∷ Parser a → Parser a
    dos p = p >>= \x → space >> return x
    dc ∷ Integral a ⇒ Parser a
    dc = dos decimal
    sdc ∷ Integral a ⇒ Parser a
    sdc = signed dc

procStat ∷ Pid → IO (Maybe ProcInfo)
procStat pid = (`catch` const (return Nothing)) $ do
    s ← bracket (openFile filename ReadMode) hClose S8.hGetContents
    return $ maybeResult $ parse parser s
  where
    filename = "/proc/" ++ show pid ++ "/stat"
