Shader "YQ_Shader/Hair"
{
	Properties
	{
		[Header(Texture)]

		_MainColor("Main Texture", 2D) = "white" {}
		_RandomTex("Random Map",2D) = "white"{} 
		_NormalMap("Normal Map", 2D) = "white" {}
		_AnisotropyTexture("Anisotropy Map", 2D) = "white" {}

		_DiffuseColor("Diffuse Color", Color) = (1,1,1,1)

		_NormalIntensity ("Normal Intensity",Range(0,2)) = 1
		_Anisotropy("Anisotropy", Range(0.0, 1.0)) = 0.5
		_LightOffset ("Light Offset",vector) = (0,0,0)

		[Header(First Specular Settings)]
		_FirstSpecularColor("First Specular Color", Color) = (1,1,1,1)
		_FirstWidth("FirstWidth", Range(0, 300)) = 2
		_FirstStrength("FirstStrength", Range(0.0, 8.0)) = 4
		_FirstOffset("First Offset", Range(-2,2)) = -0.5

		[Header(Second Specular Settings)]
		_SecondSpecularColor("Second Specular Color", Color) = (1,1,1,1)
		_SecondWidth("Second Width", Range(0.0, 300.0)) = 2
		_SecondStrength("Second Strength", Range(0.0, 8.0)) = 1.0
		_SecondOffset("_SecondOffset", Range(-2, 2)) = 0.0

		[Header(Alpha Settings)]
		_ClipValue("Clip Value", Range(0.0, 1.0)) = 0.2
		_ClipValue2 ("Clip Value2",Range(0,1)) = 0
		_Clip2MulPow ("Clip2 x:mul y:pow",vector) = (1,1,1,1)
		_AlphaIntensity ("Alpha Intensity",Range(0,5)) = 1

		[Toggle(_AdditionalLights)] _AdditionalLights("_AdditionalLights", float) = 1
		[Toggle(_EnvironmentLighting)]_EnviromentLighting("_EnvironmentLighting", float) = 1
	}

	SubShader
	{
		Pass
		{
			Name "Render opaque"
			Tags { "Queue"="AlphaTest" "RenderType" = "TransparentCutout" "IgnoreProjector" = "True" "RenderPipeline" = "UniversalPipeline"  "LightMode" = "SRPDefaultUnlit"}
			Cull off	
			ZWrite On	
			HLSLPROGRAM

			#pragma vertex Vertex
			#pragma fragment FragCutOff
			#pragma shader_feature _AdditionalLights
			#pragma shader_feature _EnvironmentLighting

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "HairBase.hlsl"

			ENDHLSL
		}

		Pass
		{
			Name "Render transparent"
			Tags { "LightMode" = "UniversalForward" "Queue"="Transparent" "RenderType" = "Transparent"  "IgnoreProjector" = "True"}
			Blend SrcAlpha OneMinusSrcAlpha 
			Cull off
			ZWrite Off
			HLSLPROGRAM

			#pragma vertex Vertex
			#pragma fragment FragAlphaBlend
			#pragma shader_feature _AdditionalLights
			#pragma shader_feature _EnvironmentLighting

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "HairBase.hlsl"

			ENDHLSL
		}

	}
}