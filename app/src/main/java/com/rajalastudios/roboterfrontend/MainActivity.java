package com.rajalastudios.roboterfrontend;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentTransaction;

import android.content.Context;
import android.os.Bundle;
import android.util.Log;
import android.view.MenuItem;
import android.widget.Button;

import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.rajalastudios.roboterfrontend.ui.fragments.ControllerFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.DisplayFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.HomeFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.LogsFragment;
import com.rajalastudios.roboterfrontend.ui.fragments.SettingsFragment;

import java.io.*;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.*;

public class MainActivity extends AppCompatActivity {
    private Button connectButton;
    public Map<String, String> settings = new HashMap<>();
    public Map<String, Boolean> boolCache = new HashMap<>();

    String ipAddress = "";
    String port = "";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        settings = loadMapFromFile("settings.ludat");
        BottomNavigationView mBottomNavigationView = (BottomNavigationView) findViewById(R.id.bottom_nav_view);

        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setChecked(true);
        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setIcon(R.drawable.round_home_24);
        loadFragment(new HomeFragment(), true);
        mBottomNavigationView.setOnNavigationItemSelectedListener(new BottomNavigationView.OnNavigationItemSelectedListener() {
            @Override
            public boolean onNavigationItemSelected(@NonNull MenuItem item) {
                int itemId = item.getItemId();

                if (itemId == R.id.nav_display) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_smart_display_24);
                    loadFragment(new DisplayFragment(), false);
                } else if (itemId == R.id.nav_controller) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_settings_remote_24);
                    loadFragment(new ControllerFragment(), false);
                } else if (itemId == R.id.nav_home) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_home_24);
                    loadFragment(new HomeFragment(), false);
                } else if (itemId == R.id.nav_logs) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_assignment_24);
                    loadFragment(new LogsFragment(), false);
                } else if (itemId == R.id.nav_settings) {
                    setNavBarIconsToDefault();
                    item.setIcon(R.drawable.round_settings_24);
                    loadFragment(new SettingsFragment(), false);
                }
                return true;
            }
        });

        connectForTrust();
    }

    public void reloadSettings(Map<String, String> settings){
        this.settings = settings;
    }

    private void loadFragment(Fragment fragment, boolean isAppInitD) {
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();

        if (isAppInitD) {
            fragmentTransaction.add(R.id.frame_layout, fragment);
        } else {
            fragmentTransaction.replace(R.id.frame_layout, fragment);
        }

        fragmentTransaction.replace(R.id.frame_layout, fragment);
        fragmentTransaction.commit();
    }

    private void setNavBarIconsToDefault() {
        BottomNavigationView mBottomNavigationView = (BottomNavigationView) findViewById(R.id.bottom_nav_view);
        mBottomNavigationView.getMenu().findItem(R.id.nav_display).setIcon(R.drawable.outline_smart_display_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_controller).setIcon(R.drawable.outline_settings_remote_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_home).setIcon(R.drawable.outline_home_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_logs).setIcon(R.drawable.outline_assignment_24);
        mBottomNavigationView.getMenu().findItem(R.id.nav_settings).setIcon(R.drawable.outline_settings_24);
    }

    private void sendData(String value) throws IOException {
        DatagramPacket sendPacket;
        DatagramSocket clientSocket = null;
        byte[] sendData;
        try {
            clientSocket = new DatagramSocket();
        } catch (SocketException e) {
            e.printStackTrace();
            return;
        }
        clientSocket.setSoTimeout(1000);
        sendData = value.getBytes();
        sendPacket = new DatagramPacket(sendData, sendData.length, InetAddress.getByName(ipAddress), Integer.parseInt(port));
        clientSocket.send(sendPacket);
    }

    public void connectForTrust() {
        if (!(ipAddress == null && port == null)) {
            try {
                sendData("TRUST");
                Log.d("INFO", "Successfully Connected");
                boolCache.put("connected", true);
            } catch (IOException e) {
                Log.d("ERROR", "Sending Failed!");
                boolCache.put("connected", false);
            }
        }
    }

    //public void saveSettings() {
    //    EditText ipAddressTextBox = (EditText) findViewById(R.id.sdasadassad);
    //    EditText portTextBox = (EditText) findViewById(R.id.portTextBoxdaaddas);
//
    //    String ipAddressText = ipAddressTextBox.getText().toString();
    //    String portText = portTextBox.getText().toString();
    //    Log.d("VALUE", "ipAddressText: " + ipAddressText);
    //    settings.put("ipAddress", ipAddressText);
    //    Log.d("VALUE", "portText: " + portText);
    //    settings.put("port", portText);
    //    saveMapToFile(settings, "settings.ludat");
    //    Log.d("INFO", "Saved Settings!");
    //}



    public Map<String, Boolean> loadBoolMapFromFile(String fileName) {
        Map<String, Boolean> map = null;
        try {
            FileInputStream fileInputStream = openFileInput(fileName);
            ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream);
            map = (Map<String, Boolean>) objectInputStream.readObject();
            objectInputStream.close();
            fileInputStream.close();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
        return map;
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

    public void saveBoolMapToFile(Map<String, Boolean> map, String fileName) {
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

    public Map<String, String> getSettings() {
        return settings;
    }
}