package com.rajalastudios.roboterfrontend.ui.fragments;

import android.content.res.Resources;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;

import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.rajalastudios.roboterfrontend.MainActivity;
import com.rajalastudios.roboterfrontend.R;
import com.rajalastudios.roboterfrontend.ui.JoystickView;

import java.util.Objects;

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

        TextView connection_status = view.findViewById(R.id.connection_status);

        if (Boolean.TRUE.equals(mainActivity.boolCache.get("connected"))) connection_status.setText(R.string.connected);
        else if (Boolean.FALSE.equals(mainActivity.boolCache.get("connected"))) connection_status.setText(R.string.disconnected);

        rebootButton.setOnClickListener(this);
        turnLeftButton.setOnClickListener(this);
        turnRightButton.setOnClickListener(this);



        // Joystick Listener
        JoystickView joystick = (JoystickView) view.findViewById(R.id.joystickView);
        joystick.setOnMoveListener(new JoystickView.OnMoveListener() {
            String previousOperation = "MRR";
            @Override
            public void onMove(int angle, int strength) {
                if (angle == 0 && strength == 0) mainActivity.sendData("MSS");
                else if (angle <= 30 || angle >= 335) {
                    if (!Objects.equals(previousOperation, "MRR")) {
                        mainActivity.sendData("MRR");
                        previousOperation = "MRR";
                    }
                }
                else if (angle < 60) {
                }
                else if (angle <= 105) {
                    if (!Objects.equals(previousOperation, "MFF")) {
                        mainActivity.sendData("MFF");
                        previousOperation = "MFF";
                    }
                }
                else if (angle < 135) {
                    if (!Objects.equals(previousOperation, "MFL")) {
                        mainActivity.sendData("MFL");
                        previousOperation = "MFL";
                    }
                }
                else if (angle <= 205) {
                    if (!Objects.equals(previousOperation, "MLL")) {
                        mainActivity.sendData("MLL");
                        previousOperation = "MLL";
                    }
                }
                else if (angle < 250) {
                    if (!Objects.equals(previousOperation, "MBL")) {
                        mainActivity.sendData("MBL");
                        previousOperation = "MBL";
                    }
                }
                else if (angle <= 295) {
                    if (!Objects.equals(previousOperation, "MBB")) {
                        mainActivity.sendData("MBB");
                        previousOperation = "MBB";
                    }
                }
                else {
                    if (!Objects.equals(previousOperation, "MBR")) {
                        mainActivity.sendData("MBR");
                        previousOperation = "MBR";
                    }
                }
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