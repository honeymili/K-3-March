using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class ShderTemaplastEditor : Editor
{
    [MenuItem("Assets/Create/Shader/Unlit URP Shader")]
    static void UnlitURPShader()
    {
        string path = AssetDatabase.GetAssetPath(Selection.activeObject);
        string templatePath = AssetDatabase.GUIDToAssetPath(path);
        string newPath = string.Format("{0}/Nwe Unlit URP Shader.shader", path);
        AssetDatabase.CopyAsset(path: templatePath , newPath);
        AssetDatabase.ImportAsset(newPath);
    }
}
