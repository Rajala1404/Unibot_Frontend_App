package com.rajalastudios.roboterfrontend.ui.fragments;

import android.content.Context;
import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import com.rajalastudios.roboterfrontend.MainActivity;
import com.rajalastudios.roboterfrontend.R;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Map;

public class HomeFragment extends Fragment {
    private Map<String, String> settings;
    private Map<String, Boolean> booleanCache;
    private MainActivity mainActivity;
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_home, container, false);

        settings = loadMapFromFile("settings.ludat");

        new MainActivity().connectForTrust();

        booleanCache = loadBoolMapFromFile("boolean_cache.ludat");

        if (booleanCache.get("connected")) {
            TextView connectedText = (TextView) view.findViewById(R.id.connection_status);
            connectedText.setText(R.string.connected);
        } else {
            TextView connectedText = (TextView) view.findViewById(R.id.connection_status);
            connectedText.setText(R.string.disconnected);
        }

        return view;
    }

    public Map<String, String> loadMapFromFile(String fileName) {
        Map<String, String> map = null;
        try {
            FileInputStream fileInputStream = getActivity().openFileInput(fileName);
            ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream);
            map = (Map<String, String>) objectInputStream.readObject();
            objectInputStream.close();
            fileInputStream.close();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
        return map;
    }


    public Map<String, Boolean> loadBoolMapFromFile(String fileName) {
        Map<String, Boolean> map = null;
        try {
            FileInputStream fileInputStream = getActivity().openFileInput(fileName);
            ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream);
            map = (Map<String, Boolean>) objectInputStream.readObject();
            objectInputStream.close();
            fileInputStream.close();
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
        }
        return map;
    }



    public void saveBoolMapToFile(Map<String, String> map, String fileName) {
        try {
            FileOutputStream fileOutputStream = getActivity().openFileOutput(fileName, Context.MODE_PRIVATE);
            ObjectOutputStream objectOutputStream = new ObjectOutputStream(fileOutputStream);
            objectOutputStream.writeObject(map);
            objectOutputStream.close();
            fileOutputStream.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}