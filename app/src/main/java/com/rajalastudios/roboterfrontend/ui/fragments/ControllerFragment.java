package com.rajalastudios.roboterfrontend.ui.fragments;

import android.content.res.Resources;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.rajalastudios.roboterfrontend.MainActivity;
import com.rajalastudios.roboterfrontend.R;
import com.rajalastudios.roboterfrontend.ui.JoystickView;

public class ControllerFragment extends Fragment implements View.OnClickListener{

    private View view;
    private MainActivity mainActivity;

    private int previousAngle = 0;
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        view = inflater.inflate(R.layout.fragment_controller, container, false);
        mainActivity = (MainActivity) getActivity();

        FloatingActionButton rebootButton = view.findViewById(R.id.rebootButton);
        FloatingActionButton turnLeftButton = view.findViewById(R.id.turnLeftButton);
        FloatingActionButton turnRightButton = view.findViewById(R.id.turnRightButton);

        rebootButton.setOnClickListener(this);
        turnLeftButton.setOnClickListener(this);
        turnRightButton.setOnClickListener(this);



        // Joystick Listener
        JoystickView joystick = (JoystickView) view.findViewById(R.id.joystickView);
        joystick.setOnMoveListener(new JoystickView.OnMoveListener() {
            @Override
            public void onMove(int angle, int strength) {
                Log.d("JOYSTICK", "Angle: " + angle);
                int smoothAngle = 0;
                if (angle == 0 && strength == 0) mainActivity.sendData("MSS");
                else if (angle <= 30 || angle >= 335) mainActivity.sendData("MRR");
                else if (angle < 60) mainActivity.sendData("MFR");
                else if (angle <= 105) mainActivity.sendData("MFF");
                else if (angle < 135) mainActivity.sendData("MFL");
                else if (angle <= 205) mainActivity.sendData("MLL");
                else if (angle < 250) mainActivity.sendData("MBL");
                else if (angle <= 295) mainActivity.sendData("MBB");
                else mainActivity.sendData("MBR");
            }
        });

        return view;
    }

    @Override
    public void onClick(View view) {
        if (view.getId() == R.id.turnLeftButton) {
            mainActivity.sendData("MTL");
        } else if (view.getId() == R.id.turnRightButton) {
            mainActivity.sendData("MTR");
        }
    }
}