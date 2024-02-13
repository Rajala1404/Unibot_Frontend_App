package com.rajalastudios.roboterfrontend.ui.fragments;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.rajalastudios.roboterfrontend.MainActivity;
import com.rajalastudios.roboterfrontend.R;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Map;

public class HomeFragment extends Fragment {
    private Map<String, String> settings;
    private MainActivity mainActivity;
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_home, container, false);

        mainActivity = new MainActivity();

        settings = mainActivity.getSettings();

        return view;
    }
}