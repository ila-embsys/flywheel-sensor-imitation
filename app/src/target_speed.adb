with Target_Speed;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

package body Target_Speed is
    procedure Update_RPM (adc_value : int) is
        current_adc : ADC_Measure := ADC_Measure'First;
        adc_range : Integer := Integer(ADC_Measure'Last) - Integer(ADC_Measure'First);
        rpm_range : Integer := Integer(RPM'Last) - Integer(RPM'First);
        adc_rpm_coefficient : Float := Float(adc_range) / Float(rpm_range);
    begin
        current_adc := ADC_Measure(Integer(adc_value));

        Current_Angular_Velocity := RPM(Integer(Float(adc_value) * adc_rpm_coefficient) + Integer(RPM'First));
        Put_Line(Current_Angular_Velocity'Image);

        exception
            when Constraint_Error => Put_Line ("Constraint_Error!");

    end Update_RPM;

begin
    Current_Angular_Velocity := RPM'First;
    
end Target_Speed;
