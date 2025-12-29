import Lake
open Lake DSL

package "LeanCcg" where
  version := v!"0.1.0"

@[default_target]
lean_lib «LeanCcg» where
  -- add library configuration options here
