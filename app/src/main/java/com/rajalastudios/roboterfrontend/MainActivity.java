package com.rajalastudios.roboterfrontend;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import java.io.*;
import java.util.*;

public class MainActivity extends AppCompatActivity {
    private Button connectButton;
    Map<String, String> settings = new HashMap<>();

    String ipAddress = "";
    String port = "";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        settings = loadMapFromFile("settings.ludat");

        connectButton = findViewById(R.id.connect_button);
        connectButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view)
            {
                Log.d("INFO", "Saving Settings!");
                saveSettings();
            }
        });
    }

    public void saveSettings() {
        EditText ipAddressTextBox = (EditText) findViewById(R.id.ipAddressTextBox);
        EditText portTextBox = (EditText) findViewById(R.id.portTextBox);

        String ipAddressText = ipAddressTextBox.getText().toString();
        String portText = portTextBox.getText().toString();
        Log.d("VALUE", "ipAddressText: " + ipAddressText);
        settings.put("ipAddress", ipAddressText);
        Log.d("VALUE", "portText: " + portText);
        settings.put("port", portText);
        saveMapToFile(settings, "settings.ludat");
        Log.d("INFO", "Saved Settings!");
    }

    public void saveMapToFile(Map<String, String> map, String fileName) {
        try {
            FileOutputStream fileOutputStream = openFileOutput(fileName, MODE_PRIVATE);
            ObjectOutputStream objectOutputStream = new ObjectOutputStream(fileOutputStream);
            objectOutputStream.writeObject(map);
            objectOutputStream.close();
            fileOutputStream.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Map<String, String> loadMapFromFile(String fileName) {
        Map<String, String> map = null;
        try {
            FileInputStream fileInputStream = openFileInput(fileName);
            ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream);
            map = (Map<String, String>) objectInputStream.readObject();
            objectInputStream.close();
            fileInputStream.close();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
        return map;
    }
}