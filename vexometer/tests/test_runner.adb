-- SPDX-License-Identifier: PMPL-1.0-or-later

pragma Ada_2022;

with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

with Vexometer.Core;             use Vexometer.Core;
with Vexometer.CII;
with Vexometer.Patterns;
with Vexometer.Probes;

procedure Test_Runner is

   procedure Assert_True (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Assert_True;

   function Approx (Left, Right : Float; Epsilon : Float := 0.001) return Boolean is
   begin
      return abs (Left - Right) <= Epsilon;
   end Approx;

   procedure Test_Core_Calculation is
      Findings : Finding_Vector;
      Scores   : Category_Score_Array;
      ISA      : Float;
      Synthetic_Finding : constant Finding := (
         Category    => Linguistic_Pathology,
         Severity    => High,
         Location    => 1,
         Length      => 14,
         Pattern_ID  => To_Unbounded_String ("unit-sycophancy"),
         Matched     => To_Unbounded_String ("great question"),
         Explanation => To_Unbounded_String ("synthetic test finding"),
         Conf        => 950 * Confidence'Small
      );
   begin
      Findings.Append (Synthetic_Finding);

      Scores := Calculate_Category_Scores (Findings, Default_Config);
      Assert_True (Approx (Scores (Linguistic_Pathology), 0.75),
         "Core: expected Linguistic_Pathology score 0.75 for one high-severity finding");
      Assert_True (Approx (Scores (Epistemic_Failure), 0.0),
         "Core: unrelated categories should remain 0.0");

      ISA := Calculate_ISA (Findings, Default_Config);
      Assert_True (ISA > 7.0 and ISA < 8.0,
         "Core: expected ISA score in (7,8) for one high LPS finding");
   end Test_Core_Calculation;

   procedure Test_CII_Detection is
      Content    : constant String := "TODO: finish this branch" & ASCII.LF
         & "return None" & ASCII.LF
         & "unimplemented!()";
      Detections : Vexometer.CII.Detection_Array;
      Metric     : Metric_Result;
   begin
      Detections := Vexometer.CII.Analyse (Content);
      Assert_True (Natural (Detections.Length) >= 1,
         "CII: expected at least one incompleteness detection");

      Metric := Vexometer.CII.Calculate (Detections, Content'Length);
      Assert_True (Float (Metric.Value) > 0.0,
         "CII: metric value should be > 0 for incomplete content");
   end Test_CII_Detection;

   procedure Test_Pattern_Engine is
      DB       : Vexometer.Patterns.Pattern_Database;
      Density  : Float;
   begin
      Vexometer.Patterns.Initialize (DB);
      Assert_True (Vexometer.Patterns.Pattern_Count (DB) > 10,
         "Patterns: expected built-in pattern database to load");

      Density := Vexometer.Patterns.Estimate_Sycophancy_Density
         ("Great question. I'd be happy to help.");
      Assert_True (Density > 0.0,
         "Patterns: expected non-zero sycophancy density on synthetic text");
   end Test_Pattern_Engine;

   procedure Test_Probe_Suite is
      Suite : Vexometer.Probes.Probe_Suite;
   begin
      Vexometer.Probes.Initialize (Suite);
      Assert_True (Vexometer.Probes.Probe_Count (Suite) >= 8,
         "Probes: expected built-in probe suite to contain at least 8 probes");

      declare
         Brevity : constant Vexometer.Probes.Behavioural_Probe :=
            Vexometer.Probes.Brevity_Probe;
      begin
         Assert_True (Brevity.Max_Length = 20,
            "Probes: brevity probe max length invariant changed unexpectedly");
      end;
   end Test_Probe_Suite;

begin
   Test_Core_Calculation;
   Test_CII_Detection;
   Test_Pattern_Engine;
   Test_Probe_Suite;

   Put_Line ("all vexometer core tests passed");

exception
   when E : others =>
      Put_Line ("vexometer tests failed: " & Exception_Information (E));
      raise;
end Test_Runner;
