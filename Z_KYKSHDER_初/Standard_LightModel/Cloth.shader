Shader "URP/Cloth"
{
	Properties
	{
		[Enum(UnityEngine.Rendering.CullMode)] _Cull("Cull Mode", float) = 2.0
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_BaseMap("BaseMap", 2D) = "white" {}
		_BaseColor("BaseColor", Color) = (1,1,1,0)
		_MaskRRoughnessGAOBAlpha(" Mask(R =Roughness, G = AO, B =Alpha)", 2D) = "white" {}
		_Roughness("Roughness", Range(0 , 1)) = 0
		_NormalMap("NormalMap", 2D) = "bump" {}
		_Normal("Normal", Float) = 1
		_Occlusion("Occlusion", Range(0 , 1)) = 1
		[Toggle]_SwitchNormalbyFace("Switch Normal by Face", Float) = 1
		_HeightMap("HeightMap", 2D) = "white" {}
		_HeightOffset("HeightOffset", Float) = 1
		_Height("Height", Float) = 1
		_ClothDFG_LUT("ClothDFG_LUT", 2D) = "white" {}
		_SheenMap("SheenMap", 2D) = "white" {}
		_SheenColor("SheenColor", Color) = (1,1,1,0)
		_SheenRoughness("SheenRoughness", Range(0 , 1)) = 0
		[Toggle(_ALPHATEST_ON)] _AlphaClip("Alpha Clipping",float) = 0.0
		_Alpha("Alpha", Float) = 1
		_Cutoff("AlphaClip", Range(0 , 1)) = 0.5
		_Dither("Dither", Range(0 , 1)) = 0
		_EnvRotation("EnvRotation", Range(0 , 360)) = 0


	
	}

		SubShader
	{
		 Tags{"RenderType" = "Opaque" "RenderPipeline" = "UniversalPipeline" "UniversalMaterialType" = "Lit" "IgnoreProjector" = "True"}
		LOD 300


		Pass
		{

			Name "Forward"
			Tags { "LightMode" = "UniversalForwardOnly" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Cull[_Cull]
			Offset 0 , 0
			ColorMask RGBA


			HLSLPROGRAM

            // -------------------------------------
            // Material Keywords
            #pragma shader_feature_local_fragment _SURFACE_TYPE_TRANSPARENT
            #pragma shader_feature_local_fragment _ALPHATEST_ON
            #pragma shader_feature_local_fragment _EMISSION

            #pragma shader_feature_local_fragment _DIFFUSE_OFF
            #pragma shader_feature_local_fragment _SPECULAR_OFF
            #pragma shader_feature_local_fragment _SH_OFF
            #pragma shader_feature_local_fragment _IBL_OFF

            // -------------------------------------
            // Universal Pipeline keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
            #pragma multi_compile_fragment _ _ADDITIONAL_LIGHT_SHADOWS
            #pragma multi_compile_fragment _ _REFLECTION_PROBE_BLENDING
            #pragma multi_compile_fragment _ _REFLECTION_PROBE_BOX_PROJECTION
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile_fragment _ _SCREEN_SPACE_OCCLUSION
            #pragma multi_compile_fragment _ _LIGHT_LAYERS
            #pragma multi_compile_fragment _ _LIGHT_COOKIES

			// -------------------------------------
			// Unity defined keywords
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON
			#pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile_fog

			//--------------------------------------
			// GPU Instancing
			#pragma multi_compile_instancing
			#pragma instancing_options renderinglayer
			#pragma multi_compile _ DOTS_INSTANCING_ON

			#pragma vertex vert
			#pragma fragment frag

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"





			inline float Pow2(float x)
			{
				return x * x;
			}
	
			inline float Pow5(float x)
			{
				return x * x * x * x * x;
			}

			inline half3 RotateDirection(half3 R, half degrees)
			{
				float3 reflUVW = R;
				half theta = degrees * PI / 180.0f;
				half costha = cos(theta);
				half sintha = sin(theta);
				reflUVW = half3(reflUVW.x * costha - reflUVW.z * sintha, reflUVW.y, reflUVW.x * sintha + reflUVW.z * costha);
				return reflUVW;
			}

			half3 AOMultiBounce(half3 BaseColor, half AO)
			{
				half3 a = 2.0404 * BaseColor - 0.3324;
				half3 b = -4.7951 * BaseColor + 0.6417;
				half3 c = 2.7552 * BaseColor + 0.6903;
				return max(AO, ((AO * a + b) * AO + c) * AO);
			}

			float3 Diffuse_Lambert(float3 DiffuseColor)
			{
				return DiffuseColor * (1 / PI);
			}

			float Vis_Cloth(float NoV, float NoL)
			{
				return saturate(1.0 / (4.0 * (NoL + NoV - NoL * NoV)));
			}

			float3 F_Schlick_UE4(float3 SpecularColor, float VoH)
			{
				float Fc = Pow5(1 - VoH);
				return saturate(50.0 * SpecularColor.g) * Fc + (1 - Fc) * SpecularColor;
			}

			float D_Charlie_Filament(float Roughness, float NoH)
			{
				// Estevez and Kulla 2017, "Production Friendly Microfacet Sheen BRDF"
				float invAlpha = 1.0 / Pow2(Roughness);
				float cos2h = NoH * NoH;
				float sin2h = max(1.0 - cos2h, 0.0078125); // 2^(-14/2), so sin2h^2 > 0 in fp16
				return (2.0 + invAlpha) * pow(sin2h, invAlpha * 0.5) / (2.0 * PI);
			}

			float GetSpecularOcclusion(float NoV, float RoughnessSq, float AO)
			{
				return saturate(pow(abs(NoV + AO), RoughnessSq) - 1 + AO);
			}

			void GetSSAO_float(float2 ScreenUV,out float SSAO)
			{
				SSAO = 1.0f;
				#ifndef SHADERGRAPH_PREVIEW
				#if defined(_SCREEN_SPACE_OCCLUSION)
					AmbientOcclusionFactor aoFactor = GetScreenSpaceAmbientOcclusion(ScreenUV);
					SSAO = aoFactor.indirectAmbientOcclusion;
				#endif
				#endif
			}

			inline float Dither4x4Bayer(int x, int y)
			{
				const float dither[16] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}

			float3 ClothBRDF(float3 DiffuseColor, float3 SheenColor, float Roughness,float SheenRoughness,float SheenDFG, float3 N, float3 V, float3 L,float3 LightColor,float Shadow)
			{
				float a2 = Pow4(Roughness);
				float3 H = normalize(L + V);
				float NoH = saturate(dot(N,H));
				float NoV = saturate(abs(dot(N,V)) + 1e-5);
				float NoL = saturate(dot(N,L));
				float VoH = saturate(dot(V,H));
				float3 Radiance = NoL * LightColor * Shadow * PI;

				float3 DiffuseLighting = Diffuse_Lambert(DiffuseColor) * Radiance;
				//#if defined(_DIFFUSE_OFF)
				//	DiffuseLighting = half3(0,0,0);
				//#endif
				// Generalized microfacet specular
				//float D = D_GGX_UE4( a2, NoH );
				//float Vis = Vis_SmithJointApprox( a2, NoV, NoL );
				float D = D_Charlie_Filament(Roughness, NoH);
				float Vis = Vis_Cloth(NoV, NoL);
				float3 F = F_Schlick_UE4(0.04, VoH);

				float3 SpecularLighting = ((D * Vis) * F) * Radiance;
				//#if defined(_SPECULAR_OFF)
				//	SpecularLighting = half3(0,0,0);
				//#endif

				float D2 = D_Charlie_Filament(SheenRoughness, NoH);
				float Vis2 = Vis_Cloth(NoV, NoL);
				float3 F2 = SheenColor;
				float3 SheenLighting = ((D2 * Vis2) * F2) * Radiance;

				float sheenScaling = 1.0 - max(max(SheenColor.r,SheenColor.g),SheenColor.b) * SheenDFG;
				DiffuseLighting *= sheenScaling;
				SpecularLighting *= sheenScaling;

				float3 DirectLighting = DiffuseLighting + SpecularLighting + SheenLighting;
				return DirectLighting;
			}

			void DirectLighting_float(float3 DiffuseColor, float3 SheenColor, float Roughness,float SheenRoughness,float SheenDFG,
								float3 WorldPos, float3 N, float3 V,out float3 DirectLighting)
			{
				DirectLighting = half3(0,0,0);
				#ifndef SHADERGRAPH_PREVIEW
				#if defined(_MAIN_LIGHT_SHADOWS_SCREEN) && !defined(_SURFACE_TYPE_TRANSPARENT)
				float4 positionCS = TransformWorldToHClip(WorldPos);
				float4 ShadowCoord = ComputeScreenPos(positionCS);
				#else
				float4 ShadowCoord = TransformWorldToShadowCoord(WorldPos);
				#endif
				float4 ShadowMask = float4(1.0,1.0,1.0,1.0);
				//主光源
				half3 DirectLighting_MainLight = half3(0,0,0);
				{
					Light light = GetMainLight(ShadowCoord,WorldPos,ShadowMask);
					half3 L = light.direction;
					half3 LightColor = light.color;
					half Shadow = light.shadowAttenuation;
					DirectLighting_MainLight = ClothBRDF(DiffuseColor,SheenColor,Roughness,SheenRoughness,SheenDFG,N,V,L,LightColor,Shadow);
				}
				//附加光源
				half3 DirectLighting_AddLight = half3(0,0,0);
				#ifdef _ADDITIONAL_LIGHTS
				uint pixelLightCount = GetAdditionalLightsCount();
				for (uint lightIndex = 0; lightIndex < pixelLightCount; ++lightIndex)
				{
					Light light = GetAdditionalLight(lightIndex,WorldPos,ShadowMask);
					half3 L = light.direction;
					half3 LightColor = light.color;
					half Shadow = light.shadowAttenuation * light.distanceAttenuation;
					DirectLighting_AddLight += ClothBRDF(DiffuseColor,SheenColor,Roughness,SheenRoughness,SheenDFG,N,V,L,LightColor,Shadow);
				}
				#endif

				DirectLighting = DirectLighting_MainLight + DirectLighting_AddLight;
				#endif
			}

			void IndirectLighting_float(float3 DiffuseColor, float3 SheenColor, float Roughness,float SheenRoughness,float ClothDFG, float SheenDFG,
										float3 WorldPos, float3 N, float3 V,float Occlusion,float EnvRotation,out float3 IndirectLighting)
			{
				IndirectLighting = half3(0,0,0);
				#ifndef SHADERGRAPH_PREVIEW
				float NoV = saturate(abs(dot(N,V)) + 1e-5);
				//SH
				float3 DiffuseAO = AOMultiBounce(DiffuseColor,Occlusion);
				float3 RadianceSH = SampleSH(N);
				float3 IndirectDiffuse = RadianceSH * DiffuseColor * DiffuseAO;
				//#if defined(_SH_OFF)
				//	IndirectDiffuse = half3(0,0,0);
				//#endif
				//IBL
				half3 R = reflect(-V,N);
				R = RotateDirection(R,EnvRotation);
				half3 SpeucularLD = GlossyEnvironmentReflection(R,WorldPos,Roughness,1.0f);
				half3 SpecularDFG = ClothDFG * 0.04f;
				float SpecularOcclusion = GetSpecularOcclusion(NoV,Pow2(Roughness),Occlusion);
				float3 SpecularAO = AOMultiBounce(float3(0.04,0.04,0.04),SpecularOcclusion);
				float3 IndirectSpecular = SpeucularLD * SpecularDFG * SpecularAO;
				//#if defined(_IBL_OFF)
				//	IndirectSpecular = half3(0,0,0);
				//#endif

				half3 SheenSpeucularLD = GlossyEnvironmentReflection(R,WorldPos,SheenRoughness,1.0f);
				half3 SheenSpecularDFG = SheenColor * SheenDFG;
				float SheenOcclusion = GetSpecularOcclusion(NoV,Pow2(SheenRoughness),Occlusion);
				float3 SheenAO = AOMultiBounce(SheenColor,SheenOcclusion);
				float3 SheenSpecular = SheenSpeucularLD * SheenSpecularDFG * SheenAO;

				float sheenScaling = 1.0 - max(max(SheenColor.r,SheenColor.g),SheenColor.b) * SheenDFG;
				IndirectDiffuse *= sheenScaling;
				IndirectSpecular *= sheenScaling;

				IndirectLighting = IndirectDiffuse + IndirectSpecular + SheenSpecular;
				#endif
			}


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normalOS : NORMAL;
				float4 texcoord : TEXCOORD0;
				float4 ase_tangent : TANGENT;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 positionCS : SV_POSITION;

				float3 worldPos : TEXCOORD0;

				float4 shadowCoord : TEXCOORD1;

				#ifdef ASE_FOG
				float fogFactor : TEXCOORD2;
				#endif
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _MaskRRoughnessGAOBAlpha_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float _Alpha;
			float _EnvRotation;
			float _Occlusion;
			float _SwitchNormalbyFace;
			float _Normal;
			float _HeightOffset;
			float _Dither;
			float _SheenRoughness;
			float _Roughness;
			float _Height;
			float _Cutoff;
			CBUFFER_END
			sampler2D _BaseMap;
			sampler2D _SheenMap;
			sampler2D _MaskRRoughnessGAOBAlpha;
			sampler2D _ClothDFG_LUT;
			sampler2D _NormalMap;
			sampler2D _HeightMap;




			VertexOutput vert(VertexInput input)
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(input);
				UNITY_TRANSFER_INSTANCE_ID(input, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float3 ase_worldTangent = TransformObjectToWorldDir(input.ase_tangent.xyz);
				o.ase_texcoord4.xyz = ase_worldTangent;
				float3 ase_worldNormal = TransformObjectToWorldNormal(input.normalOS);
				o.ase_texcoord5.xyz = ase_worldNormal;
				float ase_vertexTangentSign = input.ase_tangent.w * unity_WorldTransformParams.w;
				float3 ase_worldBitangent = cross(ase_worldNormal, ase_worldTangent) * ase_vertexTangentSign;
				o.ase_texcoord6.xyz = ase_worldBitangent;
				float4 ase_clipPos = TransformObjectToHClip((input.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord7 = screenPos;

				o.ase_texcoord3.xy = input.texcoord.xy;

				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord3.zw = 0;
				o.ase_texcoord4.w = 0;
				o.ase_texcoord5.w = 0;
				o.ase_texcoord6.w = 0;

				float3 positionWS = TransformObjectToWorld(input.vertex.xyz);
				float4 positionCS = TransformWorldToHClip(positionWS);
				o.worldPos = positionWS;
				o.positionCS = positionCS;
				return o;
			}



			half4 frag(VertexOutput IN , FRONT_FACE_TYPE ase_vface : FRONT_FACE_SEMANTIC) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);


				float3 WorldPosition = IN.worldPos;

				float4 ShadowCoords = float4(0, 0, 0, 0);

                #if defined(_MAIN_LIGHT_SHADOWS_SCREEN)
                float4 positionCS = TransformWorldToHClip(WorldPosition);
                float4 ShadowCoord = ComputeScreenPos(positionCS);
                #else
                float4 ShadowCoord = TransformWorldToShadowCoord(WorldPosition);
                #endif
				float localDirectLighting1_g45 = (0.0);
				float2 uv_BaseMap = IN.ase_texcoord3.xy * _BaseMap_ST.xy + _BaseMap_ST.zw;
				float4 BaseColor29 = (tex2D(_BaseMap, uv_BaseMap) * _BaseColor);
				float3 temp_output_6_0_g45 = BaseColor29.rgb;
				float3 DiffuseColor1_g45 = temp_output_6_0_g45;
				float2 uv_SheenMap = IN.ase_texcoord3.xy * _SheenMap_ST.xy + _SheenMap_ST.zw;
				float4 SheenColor34 = (tex2D(_SheenMap, uv_SheenMap) * _SheenColor);
				float3 temp_output_21_0_g45 = SheenColor34.rgb;
				float3 SheenColor1_g45 = temp_output_21_0_g45;
				float2 uv_MaskRRoughnessGAOBAlpha = IN.ase_texcoord3.xy * _MaskRRoughnessGAOBAlpha_ST.xy + _MaskRRoughnessGAOBAlpha_ST.zw;
				float4 tex2DNode17 = tex2D(_MaskRRoughnessGAOBAlpha, uv_MaskRRoughnessGAOBAlpha);
				float clampResult39 = clamp((tex2DNode17.r * _Roughness) , 0.0001 , 1.0);
				float Roughness40 = clampResult39;
				float temp_output_10_0_g45 = Roughness40;
				float Roughness1_g45 = temp_output_10_0_g45;
				float clampResult45 = clamp((tex2DNode17.r * _SheenRoughness) , 0.0001 , 1.0);
				float SheenRoughness46 = clampResult45;
				float temp_output_22_0_g45 = SheenRoughness46;
				float SheenRoughness1_g45 = temp_output_22_0_g45;
				float2 uv_NormalMap = IN.ase_texcoord3.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack18 = UnpackNormalScale(tex2D(_NormalMap, uv_NormalMap), _Normal);
				unpack18.z = lerp(1, unpack18.z, saturate(_Normal));
				float2 uv_HeightMap = IN.ase_texcoord3.xy * _HeightMap_ST.xy + _HeightMap_ST.zw;
				float2 temp_output_176_0_g44 = uv_HeightMap;
				float2 break183_g44 = temp_output_176_0_g44;
				float temp_output_180_0_g44 = (pow(_HeightOffset , 3.0) * 0.1);
				float2 appendResult186_g44 = (float2((break183_g44.x + temp_output_180_0_g44) , break183_g44.y));
				float4 tex2DNode189_g44 = tex2D(_HeightMap, temp_output_176_0_g44);
				float temp_output_178_0_g44 = _Height;
				float3 appendResult200_g44 = (float3(1.0 , 0.0 , ((tex2D(_HeightMap, appendResult186_g44).r - tex2DNode189_g44.r) * temp_output_178_0_g44)));
				float2 break185_g44 = temp_output_176_0_g44;
				float2 appendResult188_g44 = (float2(break185_g44.x , (temp_output_180_0_g44 + break185_g44.y)));
				float3 appendResult201_g44 = (float3(0.0 , 1.0 , (temp_output_178_0_g44 * (tex2D(_HeightMap, appendResult188_g44).r - tex2DNode189_g44.r))));
				float3 normalizeResult199_g44 = normalize(cross(appendResult200_g44 , appendResult201_g44));
				float3 temp_output_70_0 = BlendNormal(unpack18 , normalizeResult199_g44);
				float3 switchResult72 = (((ase_vface > 0) ? (temp_output_70_0) : (-temp_output_70_0)));
				float3 lerpResult74 = lerp(temp_output_70_0 , switchResult72 , _SwitchNormalbyFace);
				float3 NoramlTS49 = lerpResult74;
				float3 ase_worldTangent = IN.ase_texcoord4.xyz;
				float3 ase_worldNormal = IN.ase_texcoord5.xyz;
				float3 ase_worldBitangent = IN.ase_texcoord6.xyz;
				float3 tanToWorld0 = float3(ase_worldTangent.x, ase_worldBitangent.x, ase_worldNormal.x);
				float3 tanToWorld1 = float3(ase_worldTangent.y, ase_worldBitangent.y, ase_worldNormal.y);
				float3 tanToWorld2 = float3(ase_worldTangent.z, ase_worldBitangent.z, ase_worldNormal.z);
				float3 tanNormal79 = NoramlTS49;
				float3 worldNormal79 = float3(dot(tanToWorld0,tanNormal79), dot(tanToWorld1,tanNormal79), dot(tanToWorld2,tanNormal79));
				float3 ase_worldViewDir = (_WorldSpaceCameraPos.xyz - WorldPosition);
				ase_worldViewDir = normalize(ase_worldViewDir);
				float dotResult81 = dot(worldNormal79 , ase_worldViewDir);
				float clampResult83 = clamp(dotResult81 , 0.0 , 1.0);
				float2 appendResult99 = (float2(clampResult83 , Roughness40));
				float ClothDFG101 = tex2D(_ClothDFG_LUT, appendResult99).r;
				float temp_output_23_0_g45 = ClothDFG101;
				float SheenDFG1_g45 = temp_output_23_0_g45;
				float3 WorldPos1_g45 = WorldPosition;
				float3x3 ase_tangentToWorldFast = float3x3(ase_worldTangent.x,ase_worldBitangent.x,ase_worldNormal.x,ase_worldTangent.y,ase_worldBitangent.y,ase_worldNormal.y,ase_worldTangent.z,ase_worldBitangent.z,ase_worldNormal.z);
				float3 tangentToWorldDir16_g45 = normalize(mul(ase_tangentToWorldFast, NoramlTS49));
				float3 N1_g45 = tangentToWorldDir16_g45;
				float3 V1_g45 = ase_worldViewDir;
				float3 DirectLighting1_g45 = float3(0,0,0);
				DirectLighting_float(DiffuseColor1_g45 , SheenColor1_g45 , Roughness1_g45 , SheenRoughness1_g45 , SheenDFG1_g45 , WorldPos1_g45 , N1_g45 , V1_g45 , DirectLighting1_g45);
				float localIndirectLighting2_g45 = (0.0);
				float3 DiffuseColor2_g45 = temp_output_6_0_g45;
				float3 SheenColor2_g45 = temp_output_21_0_g45;
				float Roughness2_g45 = temp_output_10_0_g45;
				float SheenRoughness2_g45 = temp_output_22_0_g45;
				float2 appendResult85 = (float2(clampResult83 , SheenRoughness46));
				float SheenDFG88 = tex2D(_ClothDFG_LUT, appendResult85).r;
				float ClothDFG2_g45 = SheenDFG88;
				float SheenDFG2_g45 = temp_output_23_0_g45;
				float3 WorldPos2_g45 = WorldPosition;
				float3 N2_g45 = tangentToWorldDir16_g45;
				float3 V2_g45 = ase_worldViewDir;
				float localGetSSAO1_g42 = (0.0);
				float4 screenPos = IN.ase_texcoord7;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = (UNITY_NEAR_CLIP_VALUE >= 0) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 ScreenUV1_g42 = (ase_screenPosNorm).xy;
				float SSAO1_g42 = 0.0;
				GetSSAO_float(ScreenUV1_g42 , SSAO1_g42);
				float lerpResult118 = lerp(SSAO1_g42 , tex2DNode17.g , _Occlusion);
				float AO116 = lerpResult118;
				float Occlusion2_g45 = AO116;
				float EnvRotation2_g45 = _EnvRotation;
				float3 IndirectLighting2_g45 = float3(0,0,0);
				IndirectLighting_float(DiffuseColor2_g45 , SheenColor2_g45 , Roughness2_g45 , SheenRoughness2_g45 , ClothDFG2_g45 , SheenDFG2_g45 , WorldPos2_g45 , N2_g45 , V2_g45 , Occlusion2_g45 , EnvRotation2_g45 , IndirectLighting2_g45);

				float clampResult56 = clamp((tex2DNode17.b * _Alpha) , 0.0 , 1.0);
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer(fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4));
				dither58 = step(dither58, clampResult56);
				float lerpResult57 = lerp(clampResult56 , dither58 , _Dither);
				float Alpha60 = lerpResult57;

				float3 BakedAlbedo = 0;
				float3 BakedEmission = 0;
				float3 Color = (DirectLighting1_g45 + IndirectLighting2_g45);
				float Alpha = Alpha60;
				float AlphaClipThreshold = _Cutoff;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#if defined(_DBUFFER)
					ApplyDecalToBaseColor(IN.positionCS, Color);
				#endif

				#if defined(_ALPHAPREMULTIPLY_ON)
				Color *= Alpha;
				#endif


				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition(IN.positionCS.xyz, unity_LODFade.x);
				#endif

				#ifdef ASE_FOG
					Color = MixFog(Color, IN.fogFactor);
				#endif

				return half4(Color, Alpha);
			}

			ENDHLSL
		}


		Pass
		{

				Name "ShadowCaster"
				Tags { "LightMode"="ShadowCaster" }

				ZWrite On
				ZTest LEqual
				ColorMask 0
				Cull[_Cull]

				HLSLPROGRAM
				#pragma target 3.5

				// -------------------------------------
				// Material Keywords

				#pragma shader_feature_local_fragment _SURFACE_TYPE_TRANSPARENT
				#pragma shader_feature_local_fragment _ALPHATEST_ON

				//--------------------------------------
				// GPU Instancing
				#pragma multi_compile_instancing
				#pragma multi_compile _ DOTS_INSTANCING_ON

				// -------------------------------------
				// Universal Pipeline keywords

				// This is used during shadow map generation to differentiate between directional and punctual light shadows, as they use different formulas to apply Normal Bias
				#pragma multi_compile_vertex _ _CASTING_PUNCTUAL_LIGHT_SHADOW

				#pragma vertex ShadowPassVertex
				#pragma fragment ShadowPassFragment


				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
				#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"

				#if ( SHADER_API_D3D11 || SHADER_API_GLCORE || SHADER_API_GLES3 || SHADER_API_METAL || SHADER_API_VULKAN )
					#define CAN_SKIP_VPOS
				#endif

				float3 _LightDirection;
				float3 _LightPosition;

				struct VertexInput
				{
					float4 positionOS : POSITION;
					float3 normalOS : NORMAL;
					float4 texcoord : TEXCOORD0;
					UNITY_VERTEX_INPUT_INSTANCE_ID
				};

				struct VertexOutput
				{
					float4 positionCS : SV_POSITION;
					float2 uv : TEXCOORD1;
					UNITY_VERTEX_INPUT_INSTANCE_ID

				};

				float4 GetShadowPositionHClip(VertexInput input)
				{
					float3 positionWS = TransformObjectToWorld(input.positionOS.xyz);
					float3 normalWS = TransformObjectToWorldNormal(input.normalOS);

				#if _CASTING_PUNCTUAL_LIGHT_SHADOW
					float3 lightDirectionWS = normalize(_LightPosition - positionWS);
				#else
					float3 lightDirectionWS = _LightDirection;
				#endif

					float4 positionCS = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));

				#if UNITY_REVERSED_Z
					positionCS.z = min(positionCS.z, UNITY_NEAR_CLIP_VALUE);
				#else
					positionCS.z = max(positionCS.z, UNITY_NEAR_CLIP_VALUE);
				#endif

					return positionCS;
				}

				CBUFFER_START(UnityPerMaterial)
				float4 _BaseMap_ST;
				float4 _BaseColor;
				float4 _SheenMap_ST;
				float4 _SheenColor;
				float4 _MaskRRoughnessGAOBAlpha_ST;
				float4 _NormalMap_ST;
				float4 _HeightMap_ST;
				float _Alpha;
				float _EnvRotation;
				float _Occlusion;
				float _SwitchNormalbyFace;
				float _Normal;
				float _HeightOffset;
				float _Dither;
				float _SheenRoughness;
				float _Roughness;
				float _Height;
				float _Cutoff;

				CBUFFER_END
					
				TEXTURE2D(_BaseMap);
				SAMPLER(sampler_BaseMap);

				TEXTURE3D(_DitherMaskLOD);
				SAMPLER(sampler_DitherMaskLOD);



				VertexOutput ShadowPassVertex( VertexInput input )
				{
					VertexOutput output;
					UNITY_SETUP_INSTANCE_ID(input);

					output.uv = TRANSFORM_TEX(input.texcoord, _BaseMap);
					output.positionCS = GetShadowPositionHClip(input);
					return output; 

				}



				half4 ShadowPassFragment(VertexOutput input
				#if !defined( CAN_SKIP_VPOS )
				, UNITY_VPOS_TYPE vpos : VPOS
				#endif
				) : SV_TARGET
				{
					UNITY_SETUP_INSTANCE_ID(input);

					half4 BaseColorAlpha = SAMPLE_TEXTURE2D(_BaseMap,sampler_BaseMap,input.uv) * _BaseColor;
					half3 BaseColor = BaseColorAlpha.rgb;
					half BaseAlpha = BaseColorAlpha.a;
					#if defined(_ALPHATEST_ON)
						clip(BaseAlpha - _Cutoff);
					#endif

					#if defined( _SURFACE_TYPE_TRANSPARENT )
						#if defined( CAN_SKIP_VPOS )
							float2 vpos = input.positionCS;
						#endif
						half alphaRef = SAMPLE_TEXTURE3D( _DitherMaskLOD,sampler_DitherMaskLOD, float3( vpos.xy * 0.25, BaseAlpha * 0.9375 ) ).a;
						clip( alphaRef - 0.01 );
					#endif

					return 0;
				}

				ENDHLSL
			}
	}

	FallBack "Hidden/Universal Render Pipeline/FallbackError"
}
