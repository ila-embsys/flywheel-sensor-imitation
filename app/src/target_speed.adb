with Target_Speed;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

package body Target_Speed is
    procedure Update_RPM (
            adc_value : in int;
            next_tooth_time : out Float) is
        current_adc : ADC_Measure := ADC_Measure'First;
        adc_range : Integer := Integer(ADC_Measure'Last) - Integer(ADC_Measure'First);
        rpm_range : Integer := Integer(RPM'Last) - Integer(RPM'First);
        adc_rpm_coefficient : Float := Float(adc_range) / Float(rpm_range);
    begin
        current_adc := ADC_Measure(Integer(adc_value));

        Current_Angular_Velocity := RPM(Integer(Float(adc_value) * adc_rpm_coefficient) + Integer(RPM'First));

        Next_Tooth_Time := Target_Speed.Calculate_Tooth_Time(Current_Angular_Velocity);
        next_tooth_time := Next_Tooth_Time;

        Put(Current_Angular_Velocity'Image);
        Put(" rpm -> ");
        Put(Integer(Next_Tooth_Time * 1000000.0)'Image);
        Put_Line(" us");

        exception
            when Constraint_Error => Put_Line ("Constraint_Error!");

    end Update_RPM;

    function Calculate_Tooth_Time (current_speed : RPM) return Float is
        wheel_frequency : Float;
        tooth_frequency : Float;
        next_tooth_time : Float;
    begin
        wheel_frequency := Float(current_speed) / 60.0;
        tooth_frequency := wheel_frequency * Float(Tooth'Last);
        next_tooth_time := 1.0 / tooth_frequency;

        return next_tooth_time;

        exception
            when Constraint_Error =>
                Put_Line  ("Constraint_Error!");
                return next_tooth_time;

    end Calculate_Tooth_Time;

    procedure Time_To_Tooth is
    begin
        -- if (Tooth_Counter = Tooth'First) then
        --     New_Line;
        -- end if;

        if (Tooth_Counter = 40 or Tooth_Counter = 84) then
            Set_Pin(1);
            -- Put("1");
            Increment_Tooth;
            return;
        end if;

        if (Tooth_Counter = 43 or Tooth_Counter = 87) then
            Set_Pin(0);
            -- Put("0");
            Increment_Tooth;
            return;
        end if;

        if (Tooth_Counter mod 2) = 1 then
            Set_Pin(1);
            -- Put("1");
        else
            Set_Pin(0);
            -- Put("0");
        end if;

        Increment_Tooth;
        return;

    exception
        when Constraint_Error =>
            Put_Line  ("Constraint_Error!");

    end Time_To_Tooth;

    procedure Increment_Tooth is
    begin
        if (Tooth_Counter /= Tooth'Last) then
            Tooth_Counter := Tooth_Counter + 1;
        else
            Tooth_Counter := Tooth'First;
        end if;
    end Increment_Tooth;

begin
    Current_Angular_Velocity := RPM'First;
    Next_Tooth_Time := 1.0;
    Tooth_Counter := Tooth'First;
    
end Target_Speed;
