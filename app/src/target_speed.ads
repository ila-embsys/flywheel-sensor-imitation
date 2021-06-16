with Interfaces.C; use Interfaces.C;

package Target_Speed is
   -- pragma Preelaborate;
   pragma Elaborate_Body;

   type RPM is range 900 .. 8000;
   type ADC_Measure is mod 2 ** 12;

   Current_Angular_Velocity : RPM;
   Next_Tooth_Time : Float;

   procedure Update_RPM(
            adc_value : in int;
            next_tooth_time : out Float
         )
      with
         Export        => True,
         Convention    => C,
         External_Name => "update_rpm";

   function Calculate_Tooth_Time (current_speed : RPM) return Float;

private
end Target_Speed;
