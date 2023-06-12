Shader "YQ_Shader/PBR_D"
{
	Properties
	{
		_BaseColor("BaseColor", Color) = (1,1,1,0)
		_BaseMap("BaseMap", 2D) = "white" {}
		_Metallic("Metallic", Range( 0.001 , 1)) = 0
		_Roughness("Roughness", Range( 0.001 , 1)) = 0
		_Smoothness("Smoothness", Range( 0.001 , 1)) = 0
		_NormalMap("Normal Map", 2D) = "bump" {}
		_AO("AO", Range( 0 , 1)) = 0//没必要 因为要求开SSAO
		_EnvRotation("EnvRotation", Range( 0 , 360)) = 0
		_AlphaClip("Alpha Clip", Range( 0 , 1)) = 0
		_MaskRMetallicGRoughnessBAO("Mask", 2D) = "white" {}	
		[Toggle]_Window("Have Window", Int) = 0
		_Cubemap ("Reflection Cubemap", Cube) = "_Skybox" {} 
		_Stength("_Stength", Range( 0.001 , 1)) = 0.35
	}

	SubShader
	{
		LOD 0

		
		
		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		Cull Back//做背面剔除优化
		ZWrite On
		ZTest LEqual
		Offset 0 , 0 //深度偏移
		AlphaToMask Off
		
		HLSLINCLUDE
		#pragma target 3.0

		#pragma prefer_hlslcc gles
		#pragma only_renderers d3d9 d3d11 glcore gles gles3 metal vulkan //限制平台
		

		ENDHLSL

		//延迟渲染管线
		Pass
		{
			
			Name "GBuffer"
			Tags { "LightMode"="UniversalGBuffer" }
			
			Blend One Zero, One Zero
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#pragma shader_feature _WINDOW_ON
			#define ASE_FOG 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1//开启normalmap
			

			
			#pragma multi_compile _ LIGHTMAP_ON
			#pragma multi_compile _ DYNAMICLIGHTMAP_ON
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
			
			#pragma multi_compile _ _REFLECTION_PROBE_BLENDING
			#pragma multi_compile _ _REFLECTION_PROBE_BOX_PROJECTION

			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			#pragma multi_compile _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#pragma multi_compile _ _GBUFFER_NORMALS_OCT
			#pragma multi_compile _ _LIGHT_LAYERS
			#pragma multi_compile _ _RENDER_PASS_ENABLED

			#pragma vertex vert
			#pragma fragment frag

			// #define SHADERPASS SHADERPASS_GBUFFER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"


			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#include "My_Common.hlsl"



			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 normalOS : NORMAL;
				float4 tangentOS : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;

				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;

				#if defined(DYNAMICLIGHTMAP_ON)
				float2 dynamicLightmapUV : TEXCOORD7;
				#endif
				float4 texcoord : TEXCOORD8;
				float4 ase_texcoord9 : TEXCOORD9;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _BaseColor;
			float4 _BaseMap_ST;
			float4 _MaskRMetallicGRoughnessBAO_ST;
			float4 _NormalMap_ST;

			// float _CullMode;
			float _Roughness;
			float _Metallic;
			float _Smoothness;
			float _AO;
			float _EnvRotation;
			float _MactcapPow;

			float _AlphaClip;
			float _Stength;
			CBUFFER_END
			sampler2D _BaseMap;
			sampler2D _MaskRMetallicGRoughnessBAO;
			sampler2D _NormalMap;
			TEXTURECUBE(_Cubemap);
			SAMPLER(sampler_Cubemap);

			

			
			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.texcoord.xy = v.texcoord.xy;
				o.ase_texcoord9 = v.vertex;
				
				
				o.texcoord.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = defaultVertexValue;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.normalOS = v.normalOS;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.normalOS, v.tangentOS );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				#if defined(DYNAMICLIGHTMAP_ON)
				o.dynamicLightmapUV.xy = v.texcoord2.xy * unity_DynamicLightmapST.xy + unity_DynamicLightmapST.zw;
				#endif

				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				
				#endif
				
				o.clipPos = positionCS;

				return o;
			}
			

			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			

			FragmentOutput frag ( VertexOutput IN )
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				float3 Test;
				float3 WorldNormal = normalize( IN.tSpace0.xyz );
				float3 WorldTangent = IN.tSpace1.xyz;
				float3 WorldBiTangent = IN.tSpace2.xyz;

				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				ShadowCoords = TransformWorldToShadowCoord( WorldPosition );//阴影坐标

				WorldViewDirection = SafeNormalize( WorldViewDirection );//避免除以0 https://www.zhihu.com/question/64538049



	
				

				float3 temp_cast_0 = (0.0).xxx;
				
				float3 temp_cast_1 = (0.0).xxx;
				
				float localDirectLighting1_g2 = ( 0.0 );
				float2 uv_BaseMap = IN.texcoord.xy * _BaseMap_ST.xy + _BaseMap_ST.zw;
				float4 BaseMap = tex2D( _BaseMap, uv_BaseMap );
				float4 temp_output_20_0 = ( _BaseColor * BaseMap );
				float3 temp_output_6_0_g2 = temp_output_20_0.rgb;
				float2 uv_MaskRMetallicGRoughnessBAO = IN.texcoord.xy * _MaskRMetallicGRoughnessBAO_ST.xy + _MaskRMetallicGRoughnessBAO_ST.zw;

				float4 tex2DNode17 = tex2D( _MaskRMetallicGRoughnessBAO, uv_MaskRMetallicGRoughnessBAO );

				float Metallic2 = _Metallic;
				float3 lerpResult11_g2 = lerp( temp_output_6_0_g2 , float3( 0,0,0 ) , Metallic2);
				float3 DiffuseColor1_g2 = lerpResult11_g2;
				float3 temp_cast_3 = (( 0.5 * 0.08 )).xxx;
				float3 lerpResult12_g2 = lerp( temp_cast_3 , temp_output_6_0_g2 , Metallic2);
				float3 SpecularColor1_g2 = lerpResult12_g2;
				float temp_output_10_0_g2 = max( ( _Roughness * tex2DNode17.g ) , 0.001 );
				float Roughness1_g2 = temp_output_10_0_g2;
				float3 WorldPos1_g2 = WorldPosition;
				float2 uv_NormalMap = IN.texcoord.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3x3 tangentToWorldFast = float3x3(WorldTangent.x,WorldBiTangent.x,WorldNormal.x,WorldTangent.y,WorldBiTangent.y,WorldNormal.y,WorldTangent.z,WorldBiTangent.z,WorldNormal.z);
				float3 normalWs = normalize( mul( tangentToWorldFast, UnpackNormalScale( tex2D( _NormalMap, uv_NormalMap ), 1.0f ) ) );
				float3 N1_g2 = normalWs;
				float3 V1_g2 = WorldViewDirection;
				float3 DirectLighting1_g2 = float3( 0,0,0 );
				DirectLighting_float( DiffuseColor1_g2 , SpecularColor1_g2 , Roughness1_g2 , WorldPos1_g2 , N1_g2 , V1_g2 , DirectLighting1_g2 );
				float localIndirectLighting2_g2 = ( 0.0 );
				float3 DiffuseColor2_g2 = lerpResult11_g2;
				float3 SpecularColor2_g2 = lerpResult12_g2;
				float Roughness2_g2 = temp_output_10_0_g2;
				float3 WorldPos2_g2 = WorldPosition;
				// float3 N2_g2 = normalWs;
				float3 V2_g2 = WorldViewDirection;
				float lerpResult162 = lerp( 1.0 , tex2DNode17.b , _AO);
				float Occlusion2_g2 = lerpResult162;
				float EnvRotation2_g2 = _EnvRotation;
				float3 IndirectLighting2_g2 = float3( 0,0,0 );
				IndirectLighting_float( DiffuseColor2_g2 , SpecularColor2_g2 , Roughness2_g2 , WorldPos2_g2 , normalWs , V2_g2 , Occlusion2_g2 , EnvRotation2_g2 , IndirectLighting2_g2 );
				float3 temp_output_29_0 = ( DirectLighting1_g2 + IndirectLighting2_g2 );
				float3 unityObjectToViewPos87 = TransformWorldToView( TransformObjectToWorld( IN.ase_texcoord9.xyz) );
				float3 normalizeResult75 = normalize( unityObjectToViewPos87 );
				float3 break79 = cross( normalizeResult75 , mul( UNITY_MATRIX_V, float4( WorldNormal , 0.0 ) ).xyz );
				float2 appendResult81 = (float2(-break79.y , break79.x));
				float4 lerpResult148=float4( temp_output_29_0 , 0.0 );
				float3 Albedo = temp_cast_0;
				float3 Normal = temp_cast_1;
				// float Emission = lerpResult148.rgb;
				float4 Emission=float4( temp_output_29_0 , 0.0 )  ;
				float3 Specular = 0.5;
				float Metallic = _Metallic;
				float Smoothness = _Smoothness;
				float Occlusion = 0.0;
				float Alpha = BaseMap.a;
				float AlphaClipThreshold = _AlphaClip;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData = (InputData)0;
				inputData.positionWS = WorldPosition;
				inputData.positionCS = IN.clipPos;
				inputData.shadowCoord = ShadowCoords;

				inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
				Test=normalWs;


				inputData.normalWS=normalWs;
				inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				inputData.viewDirectionWS = SafeNormalize( WorldViewDirection );



				#ifdef ASE_FOG
					inputData.fogCoord = InitializeInputDataFog(float4(WorldPosition, 1.0),  IN.fogFactorAndVertexLight.x);
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				

				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#else
					#if defined(DYNAMICLIGHTMAP_ON)
						inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, IN.dynamicLightmapUV.xy, SH, inputData.normalWS);
					#else
						inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
					#endif
				#endif

				inputData.normalizedScreenSpaceUV = GetNormalizedScreenSpaceUV(IN.clipPos);
				inputData.shadowMask = SAMPLE_SHADOWMASK(IN.lightmapUVOrVertexSH.xy);

				#if defined(DEBUG_DISPLAY)
					#if defined(DYNAMICLIGHTMAP_ON)
						inputData.dynamicLightmapUV = IN.dynamicLightmapUV.xy;
						#endif
					#if defined(LIGHTMAP_ON)
						inputData.staticLightmapUV = IN.lightmapUVOrVertexSH.xy;
					#else
						inputData.vertexSH = SH;
					#endif
				#endif

				#ifdef _DBUFFER
					ApplyDecal(IN.clipPos,
						Albedo,
						Specular,
						inputData.normalWS,
						Metallic,
						Occlusion,
						Smoothness);
				#endif

				BRDFData brdfData;
				InitializeBRDFData(Albedo, _Metallic, Specular, Smoothness, Alpha, brdfData);

				Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, inputData.shadowMask);
				half4 color;
				MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, inputData.shadowMask);
				color.rgb = GlobalIllumination(brdfData, inputData.bakedGI, Occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
				color.a = Alpha;

				float4 FinalColor;
				float3 reflectWS = reflect(-WorldViewDirection, inputData.normalWS);
				float4 Skycolor = SAMPLE_TEXTURECUBE(_Cubemap, sampler_Cubemap, reflectWS);
				color.rgb=Emission+color.rgb;
				
				
				
				#ifdef _WINDOW_ON
				FinalColor =lerp(Skycolor*_Stength+Emission+color,color,1-BaseMap.a);
    			#else
				FinalColor=color;
    			#endif

				
				


				return BRDFDataToGbuffer(brdfData, inputData, Smoothness,FinalColor);
			}

			ENDHLSL
		}
		UsePass "Universal Render Pipeline/Lit/ShadowCaster"


	
	}
	
	// CustomEditor "ASEMaterialInspector"
	// Fallback "Hidden/InternalErrorShader"
	
}
