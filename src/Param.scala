package midgard

import  chisel3.util._


case class Param(
  en:       Boolean = true,

  vaBits:   Int     = 64,
  maBits:   Int     = 64,
  paBits:   Int     = 48,
  clBits:   Int     = 512,

  asidBits: Int     = 16,
  sdidBits: Int     = 12,

  pmtBits:  Int,

  tlbWays:  Int,

  vlbIdx:   Int,
  vlbWays:  Int,

  ttwNum:   Int,

  vscEn:    Boolean,
  vscSets:  Int,
  vscWays:  Int,

  vtdSets:  Int,
  vtdWays:  Int,
  dirBits:  Int,

  llcIdx:   Int,

  mlbEn:    Boolean,
  mlbWays:  Int,

  ptcEn:    Boolean,
  ptcWays:  Int,

  mrqWays:  Int,

  ctlBase:  BigInt,
  ctlSize:  BigInt,
  ctlInit:  BigInt  = 0,

  dbg:      Boolean = false) {

  val clBytes    =  clBits / 8
  val clWid      =  log2Ceil(clBytes)

  val vdnBits    =  vaBits - 3
  val mdnBits    =  maBits - 3
  val pdnBits    =  paBits - 3

  val vcnBits    =  vaBits - clWid
  val mcnBits    =  maBits - clWid
  val pcnBits    =  paBits - clWid

  val vpnBits    =  vaBits - 12
  val mpnBits    =  maBits - 12
  val ppnBits    =  paBits - 12

  val ptwLvl     = (mpnBits + (9      - 1)) / 9
  val ptwTop     =  mpnBits - (ptwLvl - 1)  * 9

  val vscBits    =  log2Ceil(vscSets)
  val vscTagBits =  mcnBits - vscBits

  val vtdBits    =  log2Ceil(vtdSets)
  val vtdTagBits =  mcnBits - vtdBits

  val ttwIdx     =  log2Ceil(ttwNum)
  val mlbIdx     =  log2Ceil(mlbWays)
  val ptcIdx     =  log2Ceil(ptcWays)
  val mrqIdx     =  log2Ceil(mrqWays)
}