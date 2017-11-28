MODULE brackets

  USE iso_c_binding

  IMPLICIT NONE

  PRIVATE

  public t_CdiParam
  type, bind(c) :: t_CdiParam
    integer(c_int) :: discipline
    integer(c_int) :: category
    integer(c_int) :: number
  end type t_CdiParam

  INTEGER, PARAMETER :: GRID_CELL   = 1
  INTEGER, PARAMETER :: GRID_VERTEX = 2
  INTEGER, PARAMETER :: GRID_EDGE   = 3

  INTEGER, PARAMETER :: GRID_UNSTRUCTURED_CELL = 1
  INTEGER, PARAMETER :: GRID_UNSTRUCTURED_VERT = 2
  INTEGER, PARAMETER :: GRID_UNSTRUCTURED_EDGE = 3

  INTEGER, PARAMETER :: GRID_REGULAR_LONLAT    = 45

  INTEGER, PARAMETER :: GRID_REFERENCE = 9

  INTEGER, PARAMETER :: ZA_GENERIC = 42


  ENUM, BIND(C)
    ENUMERATOR ::                  &
      ZA_SURFACE = 1             , &
      ZA_CLOUD_BASE              , &
      ZA_CLOUD_TOP               , &
      ZA_ISOTHERM_ZERO           , &
      ZA_REFERENCE               , &
      ZA_REFERENCE_HALF          , &
      ZA_REFERENCE_HALF_HHL      , &
      ZA_HYBRID                  , &
      ZA_HYBRID_HALF             , &
      ZA_HYBRID_HALF_HHL         , &
      ZA_DEPTH_BELOW_LAND        , &
      ZA_DEPTH_BELOW_LAND_P1     , &
      ZA_SNOW                    , &
      ZA_SNOW_HALF               , &
      ZA_PRESSURE                , &
      ZA_HEIGHT                  , &
      ZA_HEIGHT_2M               , &
      ZA_HEIGHT_10M              , &
      ZA_ALTITUDE                , &
      ZA_MEANSEA                 , &
      ZA_ISENTROPIC              , &
      ZA_TOA                     , &
      ZA_PRESSURE_800            , &
      ZA_PRESSURE_400            , &
      ZA_PRESSURE_0              , &
      ZA_DEPTH_RUNOFF_S          , &
      ZA_DEPTH_RUNOFF_G          , &
      ZA_LAKE_BOTTOM             , &
      ZA_LAKE_BOTTOM_HALF        , &
      ZA_MIX_LAYER               , &
      ZA_SEDIMENT_BOTTOM_TW      , &
      ZA_DEPTH_BELOW_SEA         , &
      ZA_DEPTH_BELOW_SEA_HALF    , &
      ZA_GENERIC_ICE             , &
      ZA_OCEAN_SEDIMENT          , &
      ZA_PRES_FL_SFC_200         , &
      ZA_PRES_FL_200_350         , &
      ZA_PRES_FL_350_550         , &
      ZA_PRES_FL_SFC_100         , &
      ZA_PRES_FL_100_245         , &
      ZA_PRES_FL_245_390         , &
      ZA_PRES_FL_390_530         , &
      ZA_ATMOSPHERE              , &
      ZA_HEIGHT_2M_LAYER         , &
      ZA_COUNT
  END ENUM

  INTEGER, PARAMETER, PUBLIC :: cdi_ZA_types(ZA_COUNT-1) = &
    [ ZA_SURFACE            , & ! ZA_SURFACE
    & ZA_CLOUD_BASE         , & ! ZA_CLOUD_BASE
    & ZA_CLOUD_TOP          , & ! ZA_CLOUD_TOP
    & ZA_ISOTHERM_ZERO      , & ! ZA_ISOTHERM_ZERO
    & ZA_REFERENCE          , & ! ZA_REFERENCE
    & ZA_REFERENCE          , & ! ZA_REFERENCE_HALF
    & ZA_REFERENCE          , & ! ZA_REFERENCE_HALF_HHL
    & ZA_HYBRID             , & ! ZA_HYBRID
    & ZA_HYBRID_HALF        , & ! ZA_HYBRID_HALF
    & ZA_HYBRID_HALF        , & ! ZA_HYBRID_HALF_HHL
    & ZA_DEPTH_BELOW_LAND   , & ! ZA_DEPTH_BELOW_LAND
    & ZA_DEPTH_BELOW_LAND   , & ! ZA_DEPTH_BELOW_LAND_P1
    & ZA_GENERIC            , & ! ZA_SNOW
    & ZA_GENERIC            , & ! ZA_SNOW_HALF
    & ZA_PRESSURE           , & ! ZA_PRESSURE
    & ZA_HEIGHT             , & ! ZA_HEIGHT
    & ZA_HEIGHT             , & ! ZA_HEIGHT_2M
    & ZA_HEIGHT             , & ! ZA_HEIGHT_10M
    & ZA_ALTITUDE           , & ! ZA_ALTITUDE
    & ZA_MEANSEA            , & ! ZA_MEANSEA
    & ZA_ISENTROPIC         , & ! ZA_ISENTROPIC
    & ZA_TOA                , & ! ZA_TOA
    & ZA_PRESSURE           , & ! ZA_PRESSURE_800
    & ZA_PRESSURE           , & ! ZA_PRESSURE_400
    & ZA_PRESSURE           , & ! ZA_PRESSURE_0
    & ZA_DEPTH_BELOW_LAND   , & ! ZA_DEPTH_RUNOFF_S
    & ZA_DEPTH_BELOW_LAND   , & ! ZA_DEPTH_RUNOFF_G
    & ZA_LAKE_BOTTOM        , & ! ZA_LAKE_BOTTOM
    & ZA_LAKE_BOTTOM        , & ! ZA_LAKE_BOTTOM_HALF
    & ZA_MIX_LAYER          , & ! ZA_MIX_LAYER
    & ZA_SEDIMENT_BOTTOM_TW , & ! ZA_SEDIMENT_BOTTOM_TW_HALF
    & ZA_DEPTH_BELOW_SEA    , & ! ZA_DEPTH_BELOW_SEA
    & ZA_DEPTH_BELOW_SEA    , & ! ZA_DEPTH_BELOW_SEA_HALF
    & ZA_GENERIC            , & ! ZA_GENERIC_ICE
    & ZA_GENERIC            , & ! ZA_OCEAN_SEDIMENT
    & ZA_PRESSURE           , & ! ZA_PRES_FL_SFC_200
    & ZA_PRESSURE           , & ! ZA_PRES_FL_200_350
    & ZA_PRESSURE           , & ! ZA_PRES_FL_350_550
    & ZA_PRESSURE           , & ! ZA_PRES_FL_SFC_100
    & ZA_PRESSURE           , & ! ZA_PRES_FL_100_245
    & ZA_PRESSURE           , & ! ZA_PRES_FL_245_390
    & ZA_PRESSURE           , & ! ZA_PRES_FL_390_530
    & ZA_ATMOSPHERE         , & ! ZA_ATMOSPHERE
    & ZA_HEIGHT               & ! ZA_HEIGHT_2M_LAYER
    & ]

CONTAINS

    SUBROUTINE dummy()

        WRITE (*,*) 'NOTHING'

    END SUBROUTINE dummy

END MODULE brackets
