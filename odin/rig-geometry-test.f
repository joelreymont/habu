\ rig-geometry-test.f - survey import and readiness renderer tests.

require lib/errors.f
require lib/string.f
require lib/test.f
require odin/rig-geometry.f

package RIGGEO-TEST

: SURVEY$ ( -- ptr u8 n )
   s" kind,logical_name,status,tx_m,ty_m,tz_m,roll_deg,pitch_deg,yaw_deg,residual,role
rig_to_truck,,estimated,0,0,0,0,0,0,,truck_mount
camera,cam_a0,estimated,0.25,0.15,0.00,0,0,-15,0.005,front_left
camera,cam_a1,estimated,0.25,-0.15,0.00,0,0,15,0.005,front_right
camera,cam_b0,estimated,-0.25,0.15,0.00,0,0,-45,0.005,rear_left
camera,cam_b1,estimated,-0.25,-0.15,0.00,0,0,45,0.005,rear_right
" ;

: LOAD-SURVEY ( -- )
   RIGGEO:RESET
   SURVEY$ RIGGEO:SURVEY-JSON
   RIGGEO:PARSE ;

: EXPECTED-SURVEY-JSON$ ( -- ptr u8 n )
   S\" {
  \"schema_version\": \"odin.rig_geometry.extrinsics_initial.v1\",
  \"rig_to_truck\": {\"status\": \"estimated\",\"translation_m\": [0,0,0],\"rotation_rpy_deg\": [0,0,0],\"residual\": null},
  \"cameras\": [
    {
      \"logical_name\": \"cam_a0\",
      \"role\": \"front_left\",
      \"camera_to_rig\": {\"status\": \"estimated\",\"translation_m\": [0.25,0.15,0.00],\"rotation_rpy_deg\": [0,0,-15],\"residual\": 0.005}
    },
    {
      \"logical_name\": \"cam_a1\",
      \"role\": \"front_right\",
      \"camera_to_rig\": {\"status\": \"estimated\",\"translation_m\": [0.25,-0.15,0.00],\"rotation_rpy_deg\": [0,0,15],\"residual\": 0.005}
    },
    {
      \"logical_name\": \"cam_b0\",
      \"role\": \"rear_left\",
      \"camera_to_rig\": {\"status\": \"estimated\",\"translation_m\": [-0.25,0.15,0.00],\"rotation_rpy_deg\": [0,0,-45],\"residual\": 0.005}
    },
    {
      \"logical_name\": \"cam_b1\",
      \"role\": \"rear_right\",
      \"camera_to_rig\": {\"status\": \"estimated\",\"translation_m\": [-0.25,-0.15,0.00],\"rotation_rpy_deg\": [0,0,45],\"residual\": 0.005}
    }
  ]
}
" ;

: TEST-SURVEY-JSON ( -- )
   RIGGEO:RESET
   SURVEY$ RIGGEO:SURVEY-JSON EXPECTED-SURVEY-JSON$ T$= ;

: TEST-READINESS-CSV ( -- )
   LOAD-SURVEY
   RIGGEO:READINESS-CSV s" scope,name,status,translation_m,rotation_rpy_deg,residual,residual_required,ready,reason
rig,rig_to_truck,estimated,[0.000000;0.000000;0.000000],[0.000000;0.000000;0.000000],,no,yes,ready
camera,cam_a0,estimated,[0.250000;0.150000;0.000000],[0.000000;0.000000;-15.000000],0.005000,yes,yes,ready
camera,cam_a1,estimated,[0.250000;-0.150000;0.000000],[0.000000;0.000000;15.000000],0.005000,yes,yes,ready
camera,cam_b0,estimated,[-0.250000;0.150000;0.000000],[0.000000;0.000000;-45.000000],0.005000,yes,yes,ready
camera,cam_b1,estimated,[-0.250000;-0.150000;0.000000],[0.000000;0.000000;45.000000],0.005000,yes,yes,ready
" T$=
   RIGGEO:RESULT s" pass" T$= ;

: TEST-READINESS-MD ( -- )
   LOAD-SURVEY
   s" fixture.json" RIGGEO:READINESS-MD s" # Rig Geometry Readiness

- source: fixture.json
- schema: odin.rig_geometry.extrinsics_initial.v1
- schema ready: yes
- cameras: 4
- camera count ready: yes
- duplicate logical names: 0
- ready camera transforms: 4/4
- ready transforms: 5/5
- result: pass

| scope | name | status | translation | rotation | residual | ready | reason |
| --- | --- | --- | --- | --- | ---: | --- | --- |
| rig | rig_to_truck | estimated | `[0.000000, 0.000000, 0.000000]` | `[0.000000, 0.000000, 0.000000]` |  | yes | ready |
| camera | cam_a0 | estimated | `[0.250000, 0.150000, 0.000000]` | `[0.000000, 0.000000, -15.000000]` | 0.005000 | yes | ready |
| camera | cam_a1 | estimated | `[0.250000, -0.150000, 0.000000]` | `[0.000000, 0.000000, 15.000000]` | 0.005000 | yes | ready |
| camera | cam_b0 | estimated | `[-0.250000, 0.150000, 0.000000]` | `[0.000000, 0.000000, -45.000000]` | 0.005000 | yes | ready |
| camera | cam_b1 | estimated | `[-0.250000, -0.150000, 0.000000]` | `[0.000000, 0.000000, 45.000000]` | 0.005000 | yes | ready |
" T$= ;

: TEST-INVALID-SURVEY ( -- )
   [: s" kind,logical_name,status,tx_m,ty_m,tz_m,roll_deg,pitch_deg,yaw_deg,residual,role
rig_to_truck,,estimated,0,0,0,0,0,0,,truck_mount
" RIGGEO:SURVEY-JSON 2drop ;] -8702 TTHROWSQ ;

: RUN ( -- )
   T-RESET
   TEST-SURVEY-JSON
   TEST-READINESS-CSV
   TEST-READINESS-MD
   TEST-INVALID-SURVEY ;

RUN
T-REPORT

end-package
