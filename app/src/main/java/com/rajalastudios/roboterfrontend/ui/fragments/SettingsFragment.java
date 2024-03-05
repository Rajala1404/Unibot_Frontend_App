package com.rajalastudios.roboterfrontend.ui.fragments;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import com.rajalastudios.roboterfrontend.MainActivity;
import com.rajalastudios.roboterfrontend.R;

import java.io.*;
import java.util.*;

public class SettingsFragment extends Fragment {
    private Button saveButton;

    private Map<String, String> settings;
    private MainActivity mainActivity;
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_settings, container, false);
        mainActivity = (MainActivity) getActivity();

        settings = mainActivity.settings;



        saveButton = view.findViewById(R.id.settings_save_button);
        saveButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view1) {
                saveSettings(view);
            }
        });

        setTextFields(view);

        return view;
    }

    private void setTextFields(View view) {
        EditText ipAddressTextBox = (EditText) view.findViewById(R.id.ipAddressTextBox);
        EditText portTextBox = (EditText) view.findViewById(R.id.portTextBox);

        if (!(settings == null)) {
            ipAddressTextBox.setText(settings.get("ipAddress"));
            portTextBox.setText(settings.get("port"));
        }

    }

    private void saveSettings(View view) {
        EditText ipAddressTextBox = (EditText) view.findViewById(R.id.ipAddressTextBox);
        EditText portTextBox = (EditText) view.findViewById(R.id.portTextBox);

        String ipAddressText = ipAddressTextBox.getText().toString();
        String portText = portTextBox.getText().toString();

        //Set Settings
        settings = new HashMap<>();

        settings.put("ipAddress", ipAddressText);
        settings.put("port", portText);

        //Save Settings
        mainActivity.settings = settings;
        mainActivity.saveSettings();

        Log.i("INFO", "Saved Settings!");
    }
}