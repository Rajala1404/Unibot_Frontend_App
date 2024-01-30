package com.rajalastudios.roboterfrontend;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import java.io.File;

public class MainActivity extends AppCompatActivity {
    private Button connectButton;

    String ipAddress = "";
    String port = "";

    File file = new File("data.ludat");

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        connectButton = findViewById(R.id.connect_button);
        connectButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view)
            {
                Log.d("INFO", "Saving Values!");
                saveValues();
            }
        });
    }

    public void saveValues() {
        EditText ipAddressTextBox = (EditText) findViewById(R.id.ipAddressTextBox);
        EditText portTextBox = (EditText) findViewById(R.id.portTextBox);

        String ipAddressText = ipAddressTextBox.getText().toString();
        String portText = portTextBox.getText().toString();
        Log.d("VALUE", "ipAddressText: " + ipAddressText);
        Log.d("VALUE", "portText: " + portText);
        Log.d("INFO", "Saved Values!");
    }
}