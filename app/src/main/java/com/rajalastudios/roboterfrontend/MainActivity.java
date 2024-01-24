package com.rajalastudios.roboterfrontend;

import androidx.appcompat.app.AppCompatActivity;

import android.os.Bundle;
import android.view.View;
import android.widget.Button;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Button connect_button = (Button)findViewById(R.id.connect_button);
    }

    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.connect_button: {
                System.out.println("Test");
            }
        }
    }
}