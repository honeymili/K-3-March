// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "BRDF/Cloth_Fur"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[Enum(UnityEngine.Rendering.CullMode)]_CullMode("CullMode", Float) = 0
		_BaseMap("BaseMap", 2D) = "white" {}
		_BaseColor("BaseColor", Color) = (1,1,1,0)
		_RoughnessMap("RoughnessMap", 2D) = "white" {}
		_Roughness("Roughness", Range( 0 , 1)) = 0
		_NormalMap("NormalMap", 2D) = "bump" {}
		_Normal("Normal", Float) = 1
		[Toggle]_SwitchNormalbyFace("Switch Normal by Face", Float) = 1
		_HeightMap("HeightMap", 2D) = "white" {}
		_HeightOffset("HeightOffset", Float) = 1
		_Height("Height", Float) = 1
		_ClothDFG_LUT("ClothDFG_LUT", 2D) = "white" {}
		_SheenMap("SheenMap", 2D) = "white" {}
		_SheenColor("SheenColor", Color) = (1,1,1,0)
		_SheenRoughness("SheenRoughness", Range( 0 , 1)) = 0
		_AlphaMap("AlphaMap", 2D) = "white" {}
		_Alpha("Alpha", Float) = 1
		_AlphaClip("AlphaClip", Range( 0 , 1)) = 0.5
		_Dither("Dither", Range( 0 , 1)) = 0
		_EnvRotation("EnvRotation", Range( 0 , 360)) = 0
		_FurLengthm("FurLengthm", Float) = 0.5
		_FurPattern("FurPattern", 2D) = "white" {}
		_EdgeFade("EdgeFade", Range( 0 , 3)) = 0
		_FurDirection("FurDirection", Vector) = (0,0,0,0)
		_FlowMap("FlowMap", 2D) = "white" {}
		_FurWind("FurWind", Vector) = (0,0,0,0)
		_FurWindInt("FurWindInt", Float) = 0
		_FurOcclusion("FurOcclusion", Range( 0 , 1)) = 0
		[HideInInspector] _texcoord( "", 2D ) = "white" {}

		[HideInInspector]_QueueOffset("_QueueOffset", Float) = 0
        [HideInInspector]_QueueControl("_QueueControl", Float) = -1
        [HideInInspector][NoScaleOffset]unity_Lightmaps("unity_Lightmaps", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset]unity_LightmapsInd("unity_LightmapsInd", 2DArray) = "" {}
        [HideInInspector][NoScaleOffset]unity_ShadowMasks("unity_ShadowMasks", 2DArray) = "" {}
		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		
		
		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		Cull [_CullMode]
		ZWrite On
		ZTest LEqual
		Offset 0 , 0
		AlphaToMask Off
		
		HLSLINCLUDE
		#pragma target 3.0

		#pragma prefer_hlslcc gles
		#pragma only_renderers d3d9 d3d11 glcore gles gles3 metal vulkan 

		#ifndef ASE_TESS_FUNCS
		#define ASE_TESS_FUNCS
		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		#endif //ASE_TESS_FUNCS

		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero, One Zero
			ColorMask RGBA
			

			HLSLPROGRAM

			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define ASE_USING_SAMPLING_MACROS 1


			#pragma multi_compile _ _SCREEN_SPACE_OCCLUSION
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS _ADDITIONAL_OFF
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ LIGHTMAP_SHADOW_MIXING
			#pragma multi_compile _ SHADOWS_SHADOWMASK

			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON
			#pragma multi_compile _ DYNAMICLIGHTMAP_ON

			#pragma multi_compile _ _REFLECTION_PROBE_BLENDING
			#pragma multi_compile _ _REFLECTION_PROBE_BOX_PROJECTION
			#pragma multi_compile _ _DBUFFER_MRT1 _DBUFFER_MRT2 _DBUFFER_MRT3
			#pragma multi_compile _ _LIGHT_LAYERS
			
			#pragma multi_compile _ _LIGHT_COOKIES
			#pragma multi_compile _ _CLUSTERED_RENDERING

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/DBuffer.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#include "../Fn_ClothLighting.hlsl"
			#include "../Fn_Common.hlsl"
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT
			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_SCREEN_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				#if defined(DYNAMICLIGHTMAP_ON)
				float2 dynamicLightmapUV : TEXCOORD7;
				#endif
				float4 ase_texcoord8 : TEXCOORD8;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _RoughnessMap_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float4 _FlowMap_ST;
			float4 _FurPattern_ST;
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _AlphaMap_ST;
			float2 _FurWind;
			float2 _FurDirection;
			float _FurWindInt;
			float _Dither;
			float _Alpha;
			float _EnvRotation;
			float _CullMode;
			float _Roughness;
			float _EdgeFade;
			float _FurOcclusion;
			float _SwitchNormalbyFace;
			float _Height;
			float _HeightOffset;
			float _Normal;
			float _FurLengthm;
			float _SheenRoughness;
			float _AlphaClip;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			TEXTURE2D(_NormalMap);
			SAMPLER(sampler_NormalMap);
			TEXTURE2D(_HeightMap);
			SAMPLER(sampler_HeightMap);
			TEXTURE2D(_BaseMap);
			SAMPLER(sampler_BaseMap);
			TEXTURE2D(_SheenMap);
			SAMPLER(sampler_SheenMap);
			TEXTURE2D(_RoughnessMap);
			SAMPLER(sampler_RoughnessMap);
			TEXTURE2D(_ClothDFG_LUT);
			SAMPLER(sampler_ClothDFG_LUT);
			TEXTURE2D(_AlphaMap);
			SAMPLER(sampler_AlphaMap);
			TEXTURE2D(_FurPattern);
			TEXTURE2D(_FlowMap);
			SAMPLER(sampler_FlowMap);
			SAMPLER(sampler_FurPattern);


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float Fur_Layer104 = v.ase_color.r;
				
				o.ase_texcoord8.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord8.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = ( v.ase_normal * 0.01 * ( Fur_Layer104 * _FurLengthm ) );
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				#if defined(LIGHTMAP_ON)
				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				#endif

				#if defined(DYNAMICLIGHTMAP_ON)
				o.dynamicLightmapUV.xy = v.texcoord2.xy * unity_DynamicLightmapST.xy + unity_DynamicLightmapST.zw;
				#endif

				#if !defined(LIGHTMAP_ON)
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );
				#endif

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
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int IN = 0; IN < 3; ++IN)
					pp[IN] = o.vertex.xyz - patch[IN].ase_normal * (dot(o.vertex.xyz, patch[IN].ase_normal) - dot(patch[IN].vertex.xyz, patch[IN].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag ( VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						, FRONT_FACE_TYPE ase_vface : FRONT_FACE_SEMANTIC ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float3 temp_cast_0 = (0.0).xxx;
				
				float2 uv_NormalMap = IN.ase_texcoord8.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack18 = UnpackNormalScale( SAMPLE_TEXTURE2D( _NormalMap, sampler_NormalMap, uv_NormalMap ), _Normal );
				unpack18.z = lerp( 1, unpack18.z, saturate(_Normal) );
				float2 uv_HeightMap = IN.ase_texcoord8.xy * _HeightMap_ST.xy + _HeightMap_ST.zw;
				float2 temp_output_176_0_g4 = uv_HeightMap;
				float2 break183_g4 = temp_output_176_0_g4;
				float temp_output_180_0_g4 = ( pow( _HeightOffset , 3.0 ) * 0.1 );
				float2 appendResult186_g4 = (float2(( break183_g4.x + temp_output_180_0_g4 ) , break183_g4.y));
				float4 tex2DNode189_g4 = SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, temp_output_176_0_g4 );
				float temp_output_178_0_g4 = _Height;
				float3 appendResult200_g4 = (float3(1.0 , 0.0 , ( ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult186_g4 ).r - tex2DNode189_g4.r ) * temp_output_178_0_g4 )));
				float2 break185_g4 = temp_output_176_0_g4;
				float2 appendResult188_g4 = (float2(break185_g4.x , ( temp_output_180_0_g4 + break185_g4.y )));
				float3 appendResult201_g4 = (float3(0.0 , 1.0 , ( temp_output_178_0_g4 * ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult188_g4 ).r - tex2DNode189_g4.r ) )));
				float3 normalizeResult199_g4 = normalize( cross( appendResult200_g4 , appendResult201_g4 ) );
				float3 temp_output_70_0 = BlendNormal( unpack18 , normalizeResult199_g4 );
				float3 switchResult72 = (((ase_vface>0)?(temp_output_70_0):(-temp_output_70_0)));
				float3 lerpResult74 = lerp( temp_output_70_0 , switchResult72 , _SwitchNormalbyFace);
				float3 NoramlTS49 = lerpResult74;
				
				float localDirectLighting1_g6 = ( 0.0 );
				float2 uv_BaseMap = IN.ase_texcoord8.xy * _BaseMap_ST.xy + _BaseMap_ST.zw;
				float Fur_Layer104 = IN.ase_color.r;
				float lerpResult154 = lerp( _FurOcclusion , 1.0 , Fur_Layer104);
				float FurOcc156 = lerpResult154;
				float4 BaseColor29 = ( SAMPLE_TEXTURE2D( _BaseMap, sampler_BaseMap, uv_BaseMap ) * _BaseColor * FurOcc156 );
				float3 temp_output_6_0_g6 = BaseColor29.rgb;
				float3 DiffuseColor1_g6 = temp_output_6_0_g6;
				float2 uv_SheenMap = IN.ase_texcoord8.xy * _SheenMap_ST.xy + _SheenMap_ST.zw;
				float4 SheenColor34 = ( SAMPLE_TEXTURE2D( _SheenMap, sampler_SheenMap, uv_SheenMap ) * _SheenColor * FurOcc156 );
				float3 temp_output_21_0_g6 = SheenColor34.rgb;
				float3 SheenColor1_g6 = temp_output_21_0_g6;
				float2 uv_RoughnessMap = IN.ase_texcoord8.xy * _RoughnessMap_ST.xy + _RoughnessMap_ST.zw;
				float4 tex2DNode17 = SAMPLE_TEXTURE2D( _RoughnessMap, sampler_RoughnessMap, uv_RoughnessMap );
				float clampResult39 = clamp( ( tex2DNode17.r * _Roughness ) , 0.001 , 1.0 );
				float Roughness40 = clampResult39;
				float temp_output_10_0_g6 = Roughness40;
				float Roughness1_g6 = temp_output_10_0_g6;
				float clampResult45 = clamp( ( tex2DNode17.r * _SheenRoughness ) , 0.001 , 1.0 );
				float SheenRoughness46 = clampResult45;
				float temp_output_22_0_g6 = SheenRoughness46;
				float SheenRoughness1_g6 = temp_output_22_0_g6;
				float3 tanToWorld0 = float3( WorldTangent.x, WorldBiTangent.x, WorldNormal.x );
				float3 tanToWorld1 = float3( WorldTangent.y, WorldBiTangent.y, WorldNormal.y );
				float3 tanToWorld2 = float3( WorldTangent.z, WorldBiTangent.z, WorldNormal.z );
				float3 tanNormal79 = NoramlTS49;
				float3 worldNormal79 = float3(dot(tanToWorld0,tanNormal79), dot(tanToWorld1,tanNormal79), dot(tanToWorld2,tanNormal79));
				float dotResult81 = dot( worldNormal79 , WorldViewDirection );
				float clampResult83 = clamp( dotResult81 , 0.0 , 1.0 );
				float2 appendResult99 = (float2(clampResult83 , Roughness40));
				float ClothDFG101 = SAMPLE_TEXTURE2D( _ClothDFG_LUT, sampler_ClothDFG_LUT, appendResult99 ).r;
				float temp_output_23_0_g6 = ClothDFG101;
				float SheenDFG1_g6 = temp_output_23_0_g6;
				float3 WorldPos1_g6 = WorldPosition;
				float3x3 tangentToWorld = float3x3(WorldTangent.x,WorldBiTangent.x,WorldNormal.x,WorldTangent.y,WorldBiTangent.y,WorldNormal.y,WorldTangent.z,WorldBiTangent.z,WorldNormal.z);
				float3 tangentToWorldDir16_g6 = normalize( mul( tangentToWorld, NoramlTS49 ) );
				float3 N1_g6 = tangentToWorldDir16_g6;
				float3 V1_g6 = WorldViewDirection;
				float3 DirectLighting1_g6 = float3( 0,0,0 );
				DirectLighting_float( DiffuseColor1_g6 , SheenColor1_g6 , Roughness1_g6 , SheenRoughness1_g6 , SheenDFG1_g6 , WorldPos1_g6 , N1_g6 , V1_g6 , DirectLighting1_g6 );
				float localIndirectLighting2_g6 = ( 0.0 );
				float3 DiffuseColor2_g6 = temp_output_6_0_g6;
				float3 SheenColor2_g6 = temp_output_21_0_g6;
				float Roughness2_g6 = temp_output_10_0_g6;
				float SheenRoughness2_g6 = temp_output_22_0_g6;
				float2 appendResult85 = (float2(clampResult83 , SheenRoughness46));
				float SheenDFG88 = SAMPLE_TEXTURE2D( _ClothDFG_LUT, sampler_ClothDFG_LUT, appendResult85 ).r;
				float ClothDFG2_g6 = SheenDFG88;
				float SheenDFG2_g6 = temp_output_23_0_g6;
				float3 WorldPos2_g6 = WorldPosition;
				float3 N2_g6 = tangentToWorldDir16_g6;
				float3 V2_g6 = WorldViewDirection;
				float localGetSSAO1_g5 = ( 0.0 );
				float4 ase_screenPosNorm = ScreenPos / ScreenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 ScreenUV1_g5 = (ase_screenPosNorm).xy;
				float SSAO1_g5 = 0.0;
				GetSSAO_float( ScreenUV1_g5 , SSAO1_g5 );
				float Occlusion2_g6 = SSAO1_g5;
				float EnvRotation2_g6 = _EnvRotation;
				float3 IndirectLighting2_g6 = float3( 0,0,0 );
				IndirectLighting_float( DiffuseColor2_g6 , SheenColor2_g6 , Roughness2_g6 , SheenRoughness2_g6 , ClothDFG2_g6 , SheenDFG2_g6 , WorldPos2_g6 , N2_g6 , V2_g6 , Occlusion2_g6 , EnvRotation2_g6 , IndirectLighting2_g6 );
				
				float3 temp_cast_3 = (0.0).xxx;
				
				float2 uv_AlphaMap = IN.ase_texcoord8.xy * _AlphaMap_ST.xy + _AlphaMap_ST.zw;
				float clampResult56 = clamp( ( SAMPLE_TEXTURE2D_LOD( _AlphaMap, sampler_AlphaMap, uv_AlphaMap, 0.0 ).r * _Alpha ) , 0.0 , 1.0 );
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer( fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4) );
				dither58 = step( dither58, clampResult56 );
				float lerpResult57 = lerp( clampResult56 , dither58 , _Dither);
				float2 uv_FurPattern = IN.ase_texcoord8.xy * _FurPattern_ST.xy + _FurPattern_ST.zw;
				float2 uv_FlowMap = IN.ase_texcoord8.xy * _FlowMap_ST.xy + _FlowMap_ST.zw;
				float2 temp_cast_4 = (1.0).xx;
				float lerpResult118 = lerp( ( ( SAMPLE_TEXTURE2D( _FurPattern, sampler_FurPattern, ( uv_FurPattern + ( _FurDirection * Fur_Layer104 * Fur_Layer104 ) + ( ( ( (SAMPLE_TEXTURE2D( _FlowMap, sampler_FlowMap, ( uv_FlowMap + ( _TimeParameters.x * _FurWind * 0.01 ) ) )).rg * 2.0 ) - temp_cast_4 ) * _FurWindInt * Fur_Layer104 ) ) ).r * 2.0 ) - ( _EdgeFade * Fur_Layer104 * Fur_Layer104 ) ) , 1.0 , step( Fur_Layer104 , 0.01 ));
				float clampResult115 = clamp( lerpResult118 , 0.0 , 1.0 );
				float Alpha60 = ( lerpResult57 * clampResult115 );
				
				float3 Albedo = temp_cast_0;
				float3 Normal = NoramlTS49;
				float3 Emission = ( DirectLighting1_g6 + IndirectLighting2_g6 );
				float3 Specular = temp_cast_3;
				float Metallic = 0;
				float Smoothness = 0.0;
				float Occlusion = 0.0;
				float Alpha = Alpha60;
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
				
				#ifdef _CLEARCOAT
				float CoatMask = 0;
				float CoatSmoothness = 0;
				#endif


				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData = (InputData)0;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					inputData.shadowCoord = ShadowCoords;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					inputData.shadowCoord = TransformWorldToShadowCoord(inputData.positionWS);
				#else
					inputData.shadowCoord = float4(0, 0, 0, 0);
				#endif


				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				#if defined(DYNAMICLIGHTMAP_ON)
				inputData.bakedGI = SAMPLE_GI(IN.lightmapUVOrVertexSH.xy, IN.dynamicLightmapUV.xy, SH, inputData.normalWS);
				#else
				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#endif

				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
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

				SurfaceData surfaceData;
				surfaceData.albedo              = Albedo;
				surfaceData.metallic            = saturate(Metallic);
				surfaceData.specular            = Specular;
				surfaceData.smoothness          = saturate(Smoothness),
				surfaceData.occlusion           = Occlusion,
				surfaceData.emission            = Emission,
				surfaceData.alpha               = saturate(Alpha);
				surfaceData.normalTS            = Normal;
				surfaceData.clearCoatMask       = 0;
				surfaceData.clearCoatSmoothness = 1;


				#ifdef _CLEARCOAT
					surfaceData.clearCoatMask       = saturate(CoatMask);
					surfaceData.clearCoatSmoothness = saturate(CoatSmoothness);
				#endif

				#ifdef _DBUFFER
					ApplyDecalToSurfaceData(IN.clipPos, surfaceData, inputData);
				#endif

				half4 color = UniversalFragmentPBR( inputData, surfaceData);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int IN = 0; IN < transPixelLightCount; ++IN)
						{
							Light light = GetAdditionalLight(IN, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int IN = 0; IN < transPixelLightCount; ++IN)
						{
							Light light = GetAdditionalLight(IN, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, float4( WorldNormal,0 ) ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos.xy ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif

				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off
			ColorMask 0

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define ASE_USING_SAMPLING_MACROS 1

			
			#pragma vertex vert
			#pragma fragment frag

			#pragma multi_compile _ _CASTING_PUNCTUAL_LIGHT_SHADOW

			#define SHADERPASS SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_NORMAL


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _RoughnessMap_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float4 _FlowMap_ST;
			float4 _FurPattern_ST;
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _AlphaMap_ST;
			float2 _FurWind;
			float2 _FurDirection;
			float _FurWindInt;
			float _Dither;
			float _Alpha;
			float _EnvRotation;
			float _CullMode;
			float _Roughness;
			float _EdgeFade;
			float _FurOcclusion;
			float _SwitchNormalbyFace;
			float _Height;
			float _HeightOffset;
			float _Normal;
			float _FurLengthm;
			float _SheenRoughness;
			float _AlphaClip;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			TEXTURE2D(_AlphaMap);
			SAMPLER(sampler_AlphaMap);
			TEXTURE2D(_FurPattern);
			TEXTURE2D(_FlowMap);
			SAMPLER(sampler_FlowMap);
			SAMPLER(sampler_FurPattern);


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			float3 _LightDirection;
			float3 _LightPosition;

			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float Fur_Layer104 = v.ase_color.r;
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = ( v.ase_normal * 0.01 * ( Fur_Layer104 * _FurLengthm ) );
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				float3 normalWS = TransformObjectToWorldDir(v.ase_normal);


			#if _CASTING_PUNCTUAL_LIGHT_SHADOW
				float3 lightDirectionWS = normalize(_LightPosition - positionWS);
			#else
				float3 lightDirectionWS = _LightDirection;
			#endif

				float4 clipPos = TransformWorldToHClip(ApplyShadowBias(positionWS, normalWS, lightDirectionWS));
			
			#if UNITY_REVERSED_Z
				clipPos.z = min(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#else
				clipPos.z = max(clipPos.z, UNITY_NEAR_CLIP_VALUE);
			#endif


				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int IN = 0; IN < 3; ++IN)
					pp[IN] = o.vertex.xyz - patch[IN].ase_normal * (dot(o.vertex.xyz, patch[IN].ase_normal) - dot(patch[IN].vertex.xyz, patch[IN].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif

			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );
				
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_AlphaMap = IN.ase_texcoord2.xy * _AlphaMap_ST.xy + _AlphaMap_ST.zw;
				float clampResult56 = clamp( ( SAMPLE_TEXTURE2D_LOD( _AlphaMap, sampler_AlphaMap, uv_AlphaMap, 0.0 ).r * _Alpha ) , 0.0 , 1.0 );
				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer( fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4) );
				dither58 = step( dither58, clampResult56 );
				float lerpResult57 = lerp( clampResult56 , dither58 , _Dither);
				float2 uv_FurPattern = IN.ase_texcoord2.xy * _FurPattern_ST.xy + _FurPattern_ST.zw;
				float Fur_Layer104 = IN.ase_color.r;
				float2 uv_FlowMap = IN.ase_texcoord2.xy * _FlowMap_ST.xy + _FlowMap_ST.zw;
				float2 temp_cast_0 = (1.0).xx;
				float lerpResult118 = lerp( ( ( SAMPLE_TEXTURE2D( _FurPattern, sampler_FurPattern, ( uv_FurPattern + ( _FurDirection * Fur_Layer104 * Fur_Layer104 ) + ( ( ( (SAMPLE_TEXTURE2D( _FlowMap, sampler_FlowMap, ( uv_FlowMap + ( _TimeParameters.x * _FurWind * 0.01 ) ) )).rg * 2.0 ) - temp_cast_0 ) * _FurWindInt * Fur_Layer104 ) ) ).r * 2.0 ) - ( _EdgeFade * Fur_Layer104 * Fur_Layer104 ) ) , 1.0 , step( Fur_Layer104 , 0.01 ));
				float clampResult115 = clamp( lerpResult118 , 0.0 , 1.0 );
				float Alpha60 = ( lerpResult57 * clampResult115 );
				
				float Alpha = Alpha60;
				float AlphaClipThreshold = _AlphaClip;
				float AlphaClipThresholdShadow = 0.5;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define ASE_USING_SAMPLING_MACROS 1

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_DEPTHONLY
        
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#define ASE_NEEDS_VERT_NORMAL


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _RoughnessMap_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float4 _FlowMap_ST;
			float4 _FurPattern_ST;
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _AlphaMap_ST;
			float2 _FurWind;
			float2 _FurDirection;
			float _FurWindInt;
			float _Dither;
			float _Alpha;
			float _EnvRotation;
			float _CullMode;
			float _Roughness;
			float _EdgeFade;
			float _FurOcclusion;
			float _SwitchNormalbyFace;
			float _Height;
			float _HeightOffset;
			float _Normal;
			float _FurLengthm;
			float _SheenRoughness;
			float _AlphaClip;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			TEXTURE2D(_AlphaMap);
			SAMPLER(sampler_AlphaMap);
			TEXTURE2D(_FurPattern);
			TEXTURE2D(_FlowMap);
			SAMPLER(sampler_FlowMap);
			SAMPLER(sampler_FurPattern);


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float Fur_Layer104 = v.ase_color.r;
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord3 = screenPos;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = ( v.ase_normal * 0.01 * ( Fur_Layer104 * _FurLengthm ) );
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int IN = 0; IN < 3; ++IN)
					pp[IN] = o.vertex.xyz - patch[IN].ase_normal * (dot(o.vertex.xyz, patch[IN].ase_normal) - dot(patch[IN].vertex.xyz, patch[IN].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						 ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_AlphaMap = IN.ase_texcoord2.xy * _AlphaMap_ST.xy + _AlphaMap_ST.zw;
				float clampResult56 = clamp( ( SAMPLE_TEXTURE2D_LOD( _AlphaMap, sampler_AlphaMap, uv_AlphaMap, 0.0 ).r * _Alpha ) , 0.0 , 1.0 );
				float4 screenPos = IN.ase_texcoord3;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer( fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4) );
				dither58 = step( dither58, clampResult56 );
				float lerpResult57 = lerp( clampResult56 , dither58 , _Dither);
				float2 uv_FurPattern = IN.ase_texcoord2.xy * _FurPattern_ST.xy + _FurPattern_ST.zw;
				float Fur_Layer104 = IN.ase_color.r;
				float2 uv_FlowMap = IN.ase_texcoord2.xy * _FlowMap_ST.xy + _FlowMap_ST.zw;
				float2 temp_cast_0 = (1.0).xx;
				float lerpResult118 = lerp( ( ( SAMPLE_TEXTURE2D( _FurPattern, sampler_FurPattern, ( uv_FurPattern + ( _FurDirection * Fur_Layer104 * Fur_Layer104 ) + ( ( ( (SAMPLE_TEXTURE2D( _FlowMap, sampler_FlowMap, ( uv_FlowMap + ( _TimeParameters.x * _FurWind * 0.01 ) ) )).rg * 2.0 ) - temp_cast_0 ) * _FurWindInt * Fur_Layer104 ) ) ).r * 2.0 ) - ( _EdgeFade * Fur_Layer104 * Fur_Layer104 ) ) , 1.0 , step( Fur_Layer104 , 0.01 ));
				float clampResult115 = clamp( lerpResult118 , 0.0 , 1.0 );
				float Alpha60 = ( lerpResult57 * clampResult115 );
				
				float Alpha = Alpha60;
				float AlphaClipThreshold = _AlphaClip;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif

				return 0;
			}
			ENDHLSL
		}
		
		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define ASE_USING_SAMPLING_MACROS 1

			
			#pragma vertex vert
			#pragma fragment frag

			#pragma shader_feature _ EDITOR_VISUALIZATION

			#define SHADERPASS SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Texture.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/TextureStack.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/Editor/ShaderGraph/Includes/ShaderPass.hlsl"

			#include "../Fn_ClothLighting.hlsl"
			#include "../Fn_Common.hlsl"
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord0 : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_tangent : TANGENT;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				#ifdef EDITOR_VISUALIZATION
				float4 VizUV : TEXCOORD2;
				float4 LightCoord : TEXCOORD3;
				#endif
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_color : COLOR;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_texcoord6 : TEXCOORD6;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_texcoord8 : TEXCOORD8;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _RoughnessMap_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float4 _FlowMap_ST;
			float4 _FurPattern_ST;
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _AlphaMap_ST;
			float2 _FurWind;
			float2 _FurDirection;
			float _FurWindInt;
			float _Dither;
			float _Alpha;
			float _EnvRotation;
			float _CullMode;
			float _Roughness;
			float _EdgeFade;
			float _FurOcclusion;
			float _SwitchNormalbyFace;
			float _Height;
			float _HeightOffset;
			float _Normal;
			float _FurLengthm;
			float _SheenRoughness;
			float _AlphaClip;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			TEXTURE2D(_BaseMap);
			SAMPLER(sampler_BaseMap);
			TEXTURE2D(_SheenMap);
			SAMPLER(sampler_SheenMap);
			TEXTURE2D(_RoughnessMap);
			SAMPLER(sampler_RoughnessMap);
			TEXTURE2D(_ClothDFG_LUT);
			TEXTURE2D(_NormalMap);
			SAMPLER(sampler_NormalMap);
			TEXTURE2D(_HeightMap);
			SAMPLER(sampler_HeightMap);
			SAMPLER(sampler_ClothDFG_LUT);
			TEXTURE2D(_AlphaMap);
			SAMPLER(sampler_AlphaMap);
			TEXTURE2D(_FurPattern);
			TEXTURE2D(_FlowMap);
			SAMPLER(sampler_FlowMap);
			SAMPLER(sampler_FurPattern);


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float Fur_Layer104 = v.ase_color.r;
				
				float3 ase_worldTangent = TransformObjectToWorldDir(v.ase_tangent.xyz);
				o.ase_texcoord5.xyz = ase_worldTangent;
				float3 ase_worldNormal = TransformObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord6.xyz = ase_worldNormal;
				float ase_vertexTangentSign = v.ase_tangent.w * unity_WorldTransformParams.w;
				float3 ase_worldBitangent = cross( ase_worldNormal, ase_worldTangent ) * ase_vertexTangentSign;
				o.ase_texcoord7.xyz = ase_worldBitangent;
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord8 = screenPos;
				
				o.ase_texcoord4.xy = v.texcoord0.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord4.zw = 0;
				o.ase_texcoord5.w = 0;
				o.ase_texcoord6.w = 0;
				o.ase_texcoord7.w = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = ( v.ase_normal * 0.01 * ( Fur_Layer104 * _FurLengthm ) );
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );

			#ifdef EDITOR_VISUALIZATION
				float2 VizUV = 0;
				float4 LightCoord = 0;
				UnityEditorVizData(v.vertex.xyz, v.texcoord0.xy, v.texcoord1.xy, v.texcoord2.xy, VizUV, LightCoord);
				o.VizUV = float4(VizUV, 0, 0);
				o.LightCoord = LightCoord;
			#endif

			#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = o.clipPos;
				o.shadowCoord = GetShadowCoord( vertexInput );
			#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord0 : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_tangent : TANGENT;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord0 = v.texcoord0;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_color = v.ase_color;
				o.ase_tangent = v.ase_tangent;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord0 = patch[0].texcoord0 * bary.x + patch[1].texcoord0 * bary.y + patch[2].texcoord0 * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int IN = 0; IN < 3; ++IN)
					pp[IN] = o.vertex.xyz - patch[IN].ase_normal * (dot(o.vertex.xyz, patch[IN].ase_normal) - dot(patch[IN].vertex.xyz, patch[IN].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN , FRONT_FACE_TYPE ase_vface : FRONT_FACE_SEMANTIC ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float3 temp_cast_0 = (0.0).xxx;
				
				float localDirectLighting1_g6 = ( 0.0 );
				float2 uv_BaseMap = IN.ase_texcoord4.xy * _BaseMap_ST.xy + _BaseMap_ST.zw;
				float Fur_Layer104 = IN.ase_color.r;
				float lerpResult154 = lerp( _FurOcclusion , 1.0 , Fur_Layer104);
				float FurOcc156 = lerpResult154;
				float4 BaseColor29 = ( SAMPLE_TEXTURE2D( _BaseMap, sampler_BaseMap, uv_BaseMap ) * _BaseColor * FurOcc156 );
				float3 temp_output_6_0_g6 = BaseColor29.rgb;
				float3 DiffuseColor1_g6 = temp_output_6_0_g6;
				float2 uv_SheenMap = IN.ase_texcoord4.xy * _SheenMap_ST.xy + _SheenMap_ST.zw;
				float4 SheenColor34 = ( SAMPLE_TEXTURE2D( _SheenMap, sampler_SheenMap, uv_SheenMap ) * _SheenColor * FurOcc156 );
				float3 temp_output_21_0_g6 = SheenColor34.rgb;
				float3 SheenColor1_g6 = temp_output_21_0_g6;
				float2 uv_RoughnessMap = IN.ase_texcoord4.xy * _RoughnessMap_ST.xy + _RoughnessMap_ST.zw;
				float4 tex2DNode17 = SAMPLE_TEXTURE2D( _RoughnessMap, sampler_RoughnessMap, uv_RoughnessMap );
				float clampResult39 = clamp( ( tex2DNode17.r * _Roughness ) , 0.001 , 1.0 );
				float Roughness40 = clampResult39;
				float temp_output_10_0_g6 = Roughness40;
				float Roughness1_g6 = temp_output_10_0_g6;
				float clampResult45 = clamp( ( tex2DNode17.r * _SheenRoughness ) , 0.001 , 1.0 );
				float SheenRoughness46 = clampResult45;
				float temp_output_22_0_g6 = SheenRoughness46;
				float SheenRoughness1_g6 = temp_output_22_0_g6;
				float2 uv_NormalMap = IN.ase_texcoord4.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack18 = UnpackNormalScale( SAMPLE_TEXTURE2D( _NormalMap, sampler_NormalMap, uv_NormalMap ), _Normal );
				unpack18.z = lerp( 1, unpack18.z, saturate(_Normal) );
				float2 uv_HeightMap = IN.ase_texcoord4.xy * _HeightMap_ST.xy + _HeightMap_ST.zw;
				float2 temp_output_176_0_g4 = uv_HeightMap;
				float2 break183_g4 = temp_output_176_0_g4;
				float temp_output_180_0_g4 = ( pow( _HeightOffset , 3.0 ) * 0.1 );
				float2 appendResult186_g4 = (float2(( break183_g4.x + temp_output_180_0_g4 ) , break183_g4.y));
				float4 tex2DNode189_g4 = SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, temp_output_176_0_g4 );
				float temp_output_178_0_g4 = _Height;
				float3 appendResult200_g4 = (float3(1.0 , 0.0 , ( ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult186_g4 ).r - tex2DNode189_g4.r ) * temp_output_178_0_g4 )));
				float2 break185_g4 = temp_output_176_0_g4;
				float2 appendResult188_g4 = (float2(break185_g4.x , ( temp_output_180_0_g4 + break185_g4.y )));
				float3 appendResult201_g4 = (float3(0.0 , 1.0 , ( temp_output_178_0_g4 * ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult188_g4 ).r - tex2DNode189_g4.r ) )));
				float3 normalizeResult199_g4 = normalize( cross( appendResult200_g4 , appendResult201_g4 ) );
				float3 temp_output_70_0 = BlendNormal( unpack18 , normalizeResult199_g4 );
				float3 switchResult72 = (((ase_vface>0)?(temp_output_70_0):(-temp_output_70_0)));
				float3 lerpResult74 = lerp( temp_output_70_0 , switchResult72 , _SwitchNormalbyFace);
				float3 NoramlTS49 = lerpResult74;
				float3 ase_worldTangent = IN.ase_texcoord5.xyz;
				float3 ase_worldNormal = IN.ase_texcoord6.xyz;
				float3 ase_worldBitangent = IN.ase_texcoord7.xyz;
				float3 tanToWorld0 = float3( ase_worldTangent.x, ase_worldBitangent.x, ase_worldNormal.x );
				float3 tanToWorld1 = float3( ase_worldTangent.y, ase_worldBitangent.y, ase_worldNormal.y );
				float3 tanToWorld2 = float3( ase_worldTangent.z, ase_worldBitangent.z, ase_worldNormal.z );
				float3 tanNormal79 = NoramlTS49;
				float3 worldNormal79 = float3(dot(tanToWorld0,tanNormal79), dot(tanToWorld1,tanNormal79), dot(tanToWorld2,tanNormal79));
				float3 worldViewDir = ( _WorldSpaceCameraPos.xyz - WorldPosition );
				worldViewDir = normalize(worldViewDir);
				float dotResult81 = dot( worldNormal79 , worldViewDir );
				float clampResult83 = clamp( dotResult81 , 0.0 , 1.0 );
				float2 appendResult99 = (float2(clampResult83 , Roughness40));
				float ClothDFG101 = SAMPLE_TEXTURE2D( _ClothDFG_LUT, sampler_ClothDFG_LUT, appendResult99 ).r;
				float temp_output_23_0_g6 = ClothDFG101;
				float SheenDFG1_g6 = temp_output_23_0_g6;
				float3 WorldPos1_g6 = WorldPosition;
				float3x3 tangentToWorld = float3x3(ase_worldTangent.x,ase_worldBitangent.x,ase_worldNormal.x,ase_worldTangent.y,ase_worldBitangent.y,ase_worldNormal.y,ase_worldTangent.z,ase_worldBitangent.z,ase_worldNormal.z);
				float3 tangentToWorldDir16_g6 = normalize( mul( tangentToWorld, NoramlTS49 ) );
				float3 N1_g6 = tangentToWorldDir16_g6;
				float3 V1_g6 = worldViewDir;
				float3 DirectLighting1_g6 = float3( 0,0,0 );
				DirectLighting_float( DiffuseColor1_g6 , SheenColor1_g6 , Roughness1_g6 , SheenRoughness1_g6 , SheenDFG1_g6 , WorldPos1_g6 , N1_g6 , V1_g6 , DirectLighting1_g6 );
				float localIndirectLighting2_g6 = ( 0.0 );
				float3 DiffuseColor2_g6 = temp_output_6_0_g6;
				float3 SheenColor2_g6 = temp_output_21_0_g6;
				float Roughness2_g6 = temp_output_10_0_g6;
				float SheenRoughness2_g6 = temp_output_22_0_g6;
				float2 appendResult85 = (float2(clampResult83 , SheenRoughness46));
				float SheenDFG88 = SAMPLE_TEXTURE2D( _ClothDFG_LUT, sampler_ClothDFG_LUT, appendResult85 ).r;
				float ClothDFG2_g6 = SheenDFG88;
				float SheenDFG2_g6 = temp_output_23_0_g6;
				float3 WorldPos2_g6 = WorldPosition;
				float3 N2_g6 = tangentToWorldDir16_g6;
				float3 V2_g6 = worldViewDir;
				float localGetSSAO1_g5 = ( 0.0 );
				float4 screenPos = IN.ase_texcoord8;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 ScreenUV1_g5 = (ase_screenPosNorm).xy;
				float SSAO1_g5 = 0.0;
				GetSSAO_float( ScreenUV1_g5 , SSAO1_g5 );
				float Occlusion2_g6 = SSAO1_g5;
				float EnvRotation2_g6 = _EnvRotation;
				float3 IndirectLighting2_g6 = float3( 0,0,0 );
				IndirectLighting_float( DiffuseColor2_g6 , SheenColor2_g6 , Roughness2_g6 , SheenRoughness2_g6 , ClothDFG2_g6 , SheenDFG2_g6 , WorldPos2_g6 , N2_g6 , V2_g6 , Occlusion2_g6 , EnvRotation2_g6 , IndirectLighting2_g6 );
				
				float2 uv_AlphaMap = IN.ase_texcoord4.xy * _AlphaMap_ST.xy + _AlphaMap_ST.zw;
				float clampResult56 = clamp( ( SAMPLE_TEXTURE2D_LOD( _AlphaMap, sampler_AlphaMap, uv_AlphaMap, 0.0 ).r * _Alpha ) , 0.0 , 1.0 );
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer( fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4) );
				dither58 = step( dither58, clampResult56 );
				float lerpResult57 = lerp( clampResult56 , dither58 , _Dither);
				float2 uv_FurPattern = IN.ase_texcoord4.xy * _FurPattern_ST.xy + _FurPattern_ST.zw;
				float2 uv_FlowMap = IN.ase_texcoord4.xy * _FlowMap_ST.xy + _FlowMap_ST.zw;
				float2 temp_cast_3 = (1.0).xx;
				float lerpResult118 = lerp( ( ( SAMPLE_TEXTURE2D( _FurPattern, sampler_FurPattern, ( uv_FurPattern + ( _FurDirection * Fur_Layer104 * Fur_Layer104 ) + ( ( ( (SAMPLE_TEXTURE2D( _FlowMap, sampler_FlowMap, ( uv_FlowMap + ( _TimeParameters.x * _FurWind * 0.01 ) ) )).rg * 2.0 ) - temp_cast_3 ) * _FurWindInt * Fur_Layer104 ) ) ).r * 2.0 ) - ( _EdgeFade * Fur_Layer104 * Fur_Layer104 ) ) , 1.0 , step( Fur_Layer104 , 0.01 ));
				float clampResult115 = clamp( lerpResult118 , 0.0 , 1.0 );
				float Alpha60 = ( lerpResult57 * clampResult115 );
				
				
				float3 Albedo = temp_cast_0;
				float3 Emission = ( DirectLighting1_g6 + IndirectLighting2_g6 );
				float Alpha = Alpha60;
				float AlphaClipThreshold = _AlphaClip;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
			#ifdef EDITOR_VISUALIZATION
				metaInput.VizUV = IN.VizUV.xy;
				metaInput.LightCoord = IN.LightCoord;
			#endif
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthNormals"
			Tags { "LightMode"="DepthNormals" }

			ZWrite On
			Blend One Zero
            ZTest LEqual
            ZWrite On

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define ASE_USING_SAMPLING_MACROS 1

			
			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS SHADERPASS_DEPTHNORMALSONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_NORMAL


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float3 worldNormal : TEXCOORD2;
				float4 worldTangent : TEXCOORD3;
				float4 ase_texcoord4 : TEXCOORD4;
				float4 ase_texcoord5 : TEXCOORD5;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _RoughnessMap_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float4 _FlowMap_ST;
			float4 _FurPattern_ST;
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _AlphaMap_ST;
			float2 _FurWind;
			float2 _FurDirection;
			float _FurWindInt;
			float _Dither;
			float _Alpha;
			float _EnvRotation;
			float _CullMode;
			float _Roughness;
			float _EdgeFade;
			float _FurOcclusion;
			float _SwitchNormalbyFace;
			float _Height;
			float _HeightOffset;
			float _Normal;
			float _FurLengthm;
			float _SheenRoughness;
			float _AlphaClip;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			TEXTURE2D(_NormalMap);
			SAMPLER(sampler_NormalMap);
			TEXTURE2D(_HeightMap);
			SAMPLER(sampler_HeightMap);
			TEXTURE2D(_AlphaMap);
			SAMPLER(sampler_AlphaMap);
			TEXTURE2D(_FurPattern);
			TEXTURE2D(_FlowMap);
			SAMPLER(sampler_FlowMap);
			SAMPLER(sampler_FurPattern);


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float Fur_Layer104 = v.ase_color.r;
				
				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord5 = screenPos;
				
				o.ase_texcoord4.xy = v.ase_texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord4.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = ( v.ase_normal * 0.01 * ( Fur_Layer104 * _FurLengthm ) );
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 normalWS = TransformObjectToWorldNormal( v.ase_normal );
				float4 tangentWS = float4(TransformObjectToWorldDir( v.ase_tangent.xyz), v.ase_tangent.w);
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.worldNormal = normalWS;
				o.worldTangent = tangentWS;

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int IN = 0; IN < 3; ++IN)
					pp[IN] = o.vertex.xyz - patch[IN].ase_normal * (dot(o.vertex.xyz, patch[IN].ase_normal) - dot(patch[IN].vertex.xyz, patch[IN].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			half4 frag(	VertexOutput IN 
						#ifdef ASE_DEPTH_WRITE_ON
						,out float outputDepth : ASE_SV_DEPTH
						#endif
						, FRONT_FACE_TYPE ase_vface : FRONT_FACE_SEMANTIC ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				
				float3 WorldNormal = IN.worldNormal;
				float4 WorldTangent = IN.worldTangent;

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_NormalMap = IN.ase_texcoord4.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack18 = UnpackNormalScale( SAMPLE_TEXTURE2D( _NormalMap, sampler_NormalMap, uv_NormalMap ), _Normal );
				unpack18.z = lerp( 1, unpack18.z, saturate(_Normal) );
				float2 uv_HeightMap = IN.ase_texcoord4.xy * _HeightMap_ST.xy + _HeightMap_ST.zw;
				float2 temp_output_176_0_g4 = uv_HeightMap;
				float2 break183_g4 = temp_output_176_0_g4;
				float temp_output_180_0_g4 = ( pow( _HeightOffset , 3.0 ) * 0.1 );
				float2 appendResult186_g4 = (float2(( break183_g4.x + temp_output_180_0_g4 ) , break183_g4.y));
				float4 tex2DNode189_g4 = SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, temp_output_176_0_g4 );
				float temp_output_178_0_g4 = _Height;
				float3 appendResult200_g4 = (float3(1.0 , 0.0 , ( ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult186_g4 ).r - tex2DNode189_g4.r ) * temp_output_178_0_g4 )));
				float2 break185_g4 = temp_output_176_0_g4;
				float2 appendResult188_g4 = (float2(break185_g4.x , ( temp_output_180_0_g4 + break185_g4.y )));
				float3 appendResult201_g4 = (float3(0.0 , 1.0 , ( temp_output_178_0_g4 * ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult188_g4 ).r - tex2DNode189_g4.r ) )));
				float3 normalizeResult199_g4 = normalize( cross( appendResult200_g4 , appendResult201_g4 ) );
				float3 temp_output_70_0 = BlendNormal( unpack18 , normalizeResult199_g4 );
				float3 switchResult72 = (((ase_vface>0)?(temp_output_70_0):(-temp_output_70_0)));
				float3 lerpResult74 = lerp( temp_output_70_0 , switchResult72 , _SwitchNormalbyFace);
				float3 NoramlTS49 = lerpResult74;
				
				float2 uv_AlphaMap = IN.ase_texcoord4.xy * _AlphaMap_ST.xy + _AlphaMap_ST.zw;
				float clampResult56 = clamp( ( SAMPLE_TEXTURE2D_LOD( _AlphaMap, sampler_AlphaMap, uv_AlphaMap, 0.0 ).r * _Alpha ) , 0.0 , 1.0 );
				float4 screenPos = IN.ase_texcoord5;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer( fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4) );
				dither58 = step( dither58, clampResult56 );
				float lerpResult57 = lerp( clampResult56 , dither58 , _Dither);
				float2 uv_FurPattern = IN.ase_texcoord4.xy * _FurPattern_ST.xy + _FurPattern_ST.zw;
				float Fur_Layer104 = IN.ase_color.r;
				float2 uv_FlowMap = IN.ase_texcoord4.xy * _FlowMap_ST.xy + _FlowMap_ST.zw;
				float2 temp_cast_0 = (1.0).xx;
				float lerpResult118 = lerp( ( ( SAMPLE_TEXTURE2D( _FurPattern, sampler_FurPattern, ( uv_FurPattern + ( _FurDirection * Fur_Layer104 * Fur_Layer104 ) + ( ( ( (SAMPLE_TEXTURE2D( _FlowMap, sampler_FlowMap, ( uv_FlowMap + ( _TimeParameters.x * _FurWind * 0.01 ) ) )).rg * 2.0 ) - temp_cast_0 ) * _FurWindInt * Fur_Layer104 ) ) ).r * 2.0 ) - ( _EdgeFade * Fur_Layer104 * Fur_Layer104 ) ) , 1.0 , step( Fur_Layer104 , 0.01 ));
				float clampResult115 = clamp( lerpResult118 , 0.0 , 1.0 );
				float Alpha60 = ( lerpResult57 * clampResult115 );
				
				float3 Normal = NoramlTS49;
				float Alpha = Alpha60;
				float AlphaClipThreshold = _AlphaClip;
				#ifdef ASE_DEPTH_WRITE_ON
				float DepthValue = 0;
				#endif

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
				outputDepth = DepthValue;
				#endif
				
				#if defined(_GBUFFER_NORMALS_OCT)
					float2 octNormalWS = PackNormalOctQuadEncode(WorldNormal);
					float2 remappedOctNormalWS = saturate(octNormalWS * 0.5 + 0.5);
					half3 packedNormalWS = PackFloat2To888(remappedOctNormalWS);
					return half4(packedNormalWS, 0.0);
				#else
					
					#if defined(_NORMALMAP)
						#if _NORMAL_DROPOFF_TS
							float crossSign = (WorldTangent.w > 0.0 ? 1.0 : -1.0) * GetOddNegativeScale();
							float3 bitangent = crossSign * cross(WorldNormal.xyz, WorldTangent.xyz);
							float3 normalWS = TransformTangentToWorld(Normal, half3x3(WorldTangent.xyz, bitangent, WorldNormal.xyz));
						#elif _NORMAL_DROPOFF_OS
							float3 normalWS = TransformObjectToWorldNormal(Normal);
						#elif _NORMAL_DROPOFF_WS
							float3 normalWS = Normal;
						#endif
					#else
						float3 normalWS = WorldNormal;
					#endif

					return half4(NormalizeNormalPerPixel(normalWS), 0.0);
				#endif
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "GBuffer"
			Tags { "LightMode"="UniversalGBuffer" }
			
			Blend One Zero, One Zero
			ColorMask RGBA
			

			HLSLPROGRAM
			
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _SPECULAR_SETUP 1
			#define _EMISSION
			#define _ALPHATEST_ON 1
			#define _NORMALMAP 1
			#define ASE_SRP_VERSION 999999
			#define ASE_USING_SAMPLING_MACROS 1

			
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

			#define SHADERPASS SHADERPASS_GBUFFER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/UnityGBuffer.hlsl"


			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#include "../Fn_ClothLighting.hlsl"
			#include "../Fn_Common.hlsl"
			#define ASE_NEEDS_VERT_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_TANGENT
			#define ASE_NEEDS_FRAG_WORLD_NORMAL
			#define ASE_NEEDS_FRAG_WORLD_BITANGENT
			#define ASE_NEEDS_FRAG_WORLD_VIEW_DIR
			#define ASE_NEEDS_FRAG_WORLD_POSITION
			#define ASE_NEEDS_FRAG_SCREEN_POSITION


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				#if defined(DYNAMICLIGHTMAP_ON)
				float2 dynamicLightmapUV : TEXCOORD7;
				#endif
				float4 ase_texcoord8 : TEXCOORD8;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _RoughnessMap_ST;
			float4 _NormalMap_ST;
			float4 _HeightMap_ST;
			float4 _FlowMap_ST;
			float4 _FurPattern_ST;
			float4 _BaseMap_ST;
			float4 _BaseColor;
			float4 _SheenMap_ST;
			float4 _SheenColor;
			float4 _AlphaMap_ST;
			float2 _FurWind;
			float2 _FurDirection;
			float _FurWindInt;
			float _Dither;
			float _Alpha;
			float _EnvRotation;
			float _CullMode;
			float _Roughness;
			float _EdgeFade;
			float _FurOcclusion;
			float _SwitchNormalbyFace;
			float _Height;
			float _HeightOffset;
			float _Normal;
			float _FurLengthm;
			float _SheenRoughness;
			float _AlphaClip;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			TEXTURE2D(_NormalMap);
			SAMPLER(sampler_NormalMap);
			TEXTURE2D(_HeightMap);
			SAMPLER(sampler_HeightMap);
			TEXTURE2D(_BaseMap);
			SAMPLER(sampler_BaseMap);
			TEXTURE2D(_SheenMap);
			SAMPLER(sampler_SheenMap);
			TEXTURE2D(_RoughnessMap);
			SAMPLER(sampler_RoughnessMap);
			TEXTURE2D(_ClothDFG_LUT);
			SAMPLER(sampler_ClothDFG_LUT);
			TEXTURE2D(_AlphaMap);
			SAMPLER(sampler_AlphaMap);
			TEXTURE2D(_FurPattern);
			TEXTURE2D(_FlowMap);
			SAMPLER(sampler_FlowMap);
			SAMPLER(sampler_FurPattern);


			inline float Dither4x4Bayer( int x, int y )
			{
				const float dither[ 16 ] = {
			 1,  9,  3, 11,
			13,  5, 15,  7,
			 4, 12,  2, 10,
			16,  8, 14,  6 };
				int r = y * 4 + x;
				return dither[r] / 16; // same # of instructions as pre-dividing due to compiler magic
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float Fur_Layer104 = v.ase_color.r;
				
				o.ase_texcoord8.xy = v.texcoord.xy;
				o.ase_color = v.ase_color;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord8.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = ( v.ase_normal * 0.01 * ( Fur_Layer104 * _FurLengthm ) );
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

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
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int IN = 0; IN < 3; ++IN)
					pp[IN] = o.vertex.xyz - patch[IN].ase_normal * (dot(o.vertex.xyz, patch[IN].ase_normal) - dot(patch[IN].vertex.xyz, patch[IN].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			#if defined(ASE_EARLY_Z_DEPTH_OPTIMIZE)
				#define ASE_SV_DEPTH SV_DepthLessEqual  
			#else
				#define ASE_SV_DEPTH SV_Depth
			#endif
			FragmentOutput frag ( VertexOutput IN 
								#ifdef ASE_DEPTH_WRITE_ON
								,out float outputDepth : ASE_SV_DEPTH
								#endif
								, FRONT_FACE_TYPE ase_vface : FRONT_FACE_SEMANTIC )
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#else
					ShadowCoords = float4(0, 0, 0, 0);
				#endif


	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float3 temp_cast_0 = (0.0).xxx;
				
				float2 uv_NormalMap = IN.ase_texcoord8.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;
				float3 unpack18 = UnpackNormalScale( SAMPLE_TEXTURE2D( _NormalMap, sampler_NormalMap, uv_NormalMap ), _Normal );
				unpack18.z = lerp( 1, unpack18.z, saturate(_Normal) );
				float2 uv_HeightMap = IN.ase_texcoord8.xy * _HeightMap_ST.xy + _HeightMap_ST.zw;
				float2 temp_output_176_0_g4 = uv_HeightMap;
				float2 break183_g4 = temp_output_176_0_g4;
				float temp_output_180_0_g4 = ( pow( _HeightOffset , 3.0 ) * 0.1 );
				float2 appendResult186_g4 = (float2(( break183_g4.x + temp_output_180_0_g4 ) , break183_g4.y));
				float4 tex2DNode189_g4 = SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, temp_output_176_0_g4 );
				float temp_output_178_0_g4 = _Height;
				float3 appendResult200_g4 = (float3(1.0 , 0.0 , ( ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult186_g4 ).r - tex2DNode189_g4.r ) * temp_output_178_0_g4 )));
				float2 break185_g4 = temp_output_176_0_g4;
				float2 appendResult188_g4 = (float2(break185_g4.x , ( temp_output_180_0_g4 + break185_g4.y )));
				float3 appendResult201_g4 = (float3(0.0 , 1.0 , ( temp_output_178_0_g4 * ( SAMPLE_TEXTURE2D( _HeightMap, sampler_HeightMap, appendResult188_g4 ).r - tex2DNode189_g4.r ) )));
				float3 normalizeResult199_g4 = normalize( cross( appendResult200_g4 , appendResult201_g4 ) );
				float3 temp_output_70_0 = BlendNormal( unpack18 , normalizeResult199_g4 );
				float3 switchResult72 = (((ase_vface>0)?(temp_output_70_0):(-temp_output_70_0)));
				float3 lerpResult74 = lerp( temp_output_70_0 , switchResult72 , _SwitchNormalbyFace);
				float3 NoramlTS49 = lerpResult74;
				
				float localDirectLighting1_g6 = ( 0.0 );
				float2 uv_BaseMap = IN.ase_texcoord8.xy * _BaseMap_ST.xy + _BaseMap_ST.zw;
				float Fur_Layer104 = IN.ase_color.r;
				float lerpResult154 = lerp( _FurOcclusion , 1.0 , Fur_Layer104);
				float FurOcc156 = lerpResult154;
				float4 BaseColor29 = ( SAMPLE_TEXTURE2D( _BaseMap, sampler_BaseMap, uv_BaseMap ) * _BaseColor * FurOcc156 );
				float3 temp_output_6_0_g6 = BaseColor29.rgb;
				float3 DiffuseColor1_g6 = temp_output_6_0_g6;
				float2 uv_SheenMap = IN.ase_texcoord8.xy * _SheenMap_ST.xy + _SheenMap_ST.zw;
				float4 SheenColor34 = ( SAMPLE_TEXTURE2D( _SheenMap, sampler_SheenMap, uv_SheenMap ) * _SheenColor * FurOcc156 );
				float3 temp_output_21_0_g6 = SheenColor34.rgb;
				float3 SheenColor1_g6 = temp_output_21_0_g6;
				float2 uv_RoughnessMap = IN.ase_texcoord8.xy * _RoughnessMap_ST.xy + _RoughnessMap_ST.zw;
				float4 tex2DNode17 = SAMPLE_TEXTURE2D( _RoughnessMap, sampler_RoughnessMap, uv_RoughnessMap );
				float clampResult39 = clamp( ( tex2DNode17.r * _Roughness ) , 0.001 , 1.0 );
				float Roughness40 = clampResult39;
				float temp_output_10_0_g6 = Roughness40;
				float Roughness1_g6 = temp_output_10_0_g6;
				float clampResult45 = clamp( ( tex2DNode17.r * _SheenRoughness ) , 0.001 , 1.0 );
				float SheenRoughness46 = clampResult45;
				float temp_output_22_0_g6 = SheenRoughness46;
				float SheenRoughness1_g6 = temp_output_22_0_g6;
				float3 tanToWorld0 = float3( WorldTangent.x, WorldBiTangent.x, WorldNormal.x );
				float3 tanToWorld1 = float3( WorldTangent.y, WorldBiTangent.y, WorldNormal.y );
				float3 tanToWorld2 = float3( WorldTangent.z, WorldBiTangent.z, WorldNormal.z );
				float3 tanNormal79 = NoramlTS49;
				float3 worldNormal79 = float3(dot(tanToWorld0,tanNormal79), dot(tanToWorld1,tanNormal79), dot(tanToWorld2,tanNormal79));
				float dotResult81 = dot( worldNormal79 , WorldViewDirection );
				float clampResult83 = clamp( dotResult81 , 0.0 , 1.0 );
				float2 appendResult99 = (float2(clampResult83 , Roughness40));
				float ClothDFG101 = SAMPLE_TEXTURE2D( _ClothDFG_LUT, sampler_ClothDFG_LUT, appendResult99 ).r;
				float temp_output_23_0_g6 = ClothDFG101;
				float SheenDFG1_g6 = temp_output_23_0_g6;
				float3 WorldPos1_g6 = WorldPosition;
				float3x3 tangentToWorld = float3x3(WorldTangent.x,WorldBiTangent.x,WorldNormal.x,WorldTangent.y,WorldBiTangent.y,WorldNormal.y,WorldTangent.z,WorldBiTangent.z,WorldNormal.z);
				float3 tangentToWorldDir16_g6 = normalize( mul( tangentToWorld, NoramlTS49 ) );
				float3 N1_g6 = tangentToWorldDir16_g6;
				float3 V1_g6 = WorldViewDirection;
				float3 DirectLighting1_g6 = float3( 0,0,0 );
				DirectLighting_float( DiffuseColor1_g6 , SheenColor1_g6 , Roughness1_g6 , SheenRoughness1_g6 , SheenDFG1_g6 , WorldPos1_g6 , N1_g6 , V1_g6 , DirectLighting1_g6 );
				float localIndirectLighting2_g6 = ( 0.0 );
				float3 DiffuseColor2_g6 = temp_output_6_0_g6;
				float3 SheenColor2_g6 = temp_output_21_0_g6;
				float Roughness2_g6 = temp_output_10_0_g6;
				float SheenRoughness2_g6 = temp_output_22_0_g6;
				float2 appendResult85 = (float2(clampResult83 , SheenRoughness46));
				float SheenDFG88 = SAMPLE_TEXTURE2D( _ClothDFG_LUT, sampler_ClothDFG_LUT, appendResult85 ).r;
				float ClothDFG2_g6 = SheenDFG88;
				float SheenDFG2_g6 = temp_output_23_0_g6;
				float3 WorldPos2_g6 = WorldPosition;
				float3 N2_g6 = tangentToWorldDir16_g6;
				float3 V2_g6 = WorldViewDirection;
				float localGetSSAO1_g5 = ( 0.0 );
				float4 ase_screenPosNorm = ScreenPos / ScreenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 ScreenUV1_g5 = (ase_screenPosNorm).xy;
				float SSAO1_g5 = 0.0;
				GetSSAO_float( ScreenUV1_g5 , SSAO1_g5 );
				float Occlusion2_g6 = SSAO1_g5;
				float EnvRotation2_g6 = _EnvRotation;
				float3 IndirectLighting2_g6 = float3( 0,0,0 );
				IndirectLighting_float( DiffuseColor2_g6 , SheenColor2_g6 , Roughness2_g6 , SheenRoughness2_g6 , ClothDFG2_g6 , SheenDFG2_g6 , WorldPos2_g6 , N2_g6 , V2_g6 , Occlusion2_g6 , EnvRotation2_g6 , IndirectLighting2_g6 );
				
				float3 temp_cast_3 = (0.0).xxx;
				
				float2 uv_AlphaMap = IN.ase_texcoord8.xy * _AlphaMap_ST.xy + _AlphaMap_ST.zw;
				float clampResult56 = clamp( ( SAMPLE_TEXTURE2D_LOD( _AlphaMap, sampler_AlphaMap, uv_AlphaMap, 0.0 ).r * _Alpha ) , 0.0 , 1.0 );
				float2 clipScreen58 = ase_screenPosNorm.xy * _ScreenParams.xy;
				float dither58 = Dither4x4Bayer( fmod(clipScreen58.x, 4), fmod(clipScreen58.y, 4) );
				dither58 = step( dither58, clampResult56 );
				float lerpResult57 = lerp( clampResult56 , dither58 , _Dither);
				float2 uv_FurPattern = IN.ase_texcoord8.xy * _FurPattern_ST.xy + _FurPattern_ST.zw;
				float2 uv_FlowMap = IN.ase_texcoord8.xy * _FlowMap_ST.xy + _FlowMap_ST.zw;
				float2 temp_cast_4 = (1.0).xx;
				float lerpResult118 = lerp( ( ( SAMPLE_TEXTURE2D( _FurPattern, sampler_FurPattern, ( uv_FurPattern + ( _FurDirection * Fur_Layer104 * Fur_Layer104 ) + ( ( ( (SAMPLE_TEXTURE2D( _FlowMap, sampler_FlowMap, ( uv_FlowMap + ( _TimeParameters.x * _FurWind * 0.01 ) ) )).rg * 2.0 ) - temp_cast_4 ) * _FurWindInt * Fur_Layer104 ) ) ).r * 2.0 ) - ( _EdgeFade * Fur_Layer104 * Fur_Layer104 ) ) , 1.0 , step( Fur_Layer104 , 0.01 ));
				float clampResult115 = clamp( lerpResult118 , 0.0 , 1.0 );
				float Alpha60 = ( lerpResult57 * clampResult115 );
				
				float3 Albedo = temp_cast_0;
				float3 Normal = NoramlTS49;
				float3 Emission = ( DirectLighting1_g6 + IndirectLighting2_g6 );
				float3 Specular = temp_cast_3;
				float Metallic = 0;
				float Smoothness = 0.0;
				float Occlusion = 0.0;
				float Alpha = Alpha60;
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



				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
				#else
					inputData.normalWS = WorldNormal;
				#endif
					
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
				InitializeBRDFData
				(Albedo, Metallic, Specular, Smoothness, Alpha, brdfData);

				Light mainLight = GetMainLight(inputData.shadowCoord, inputData.positionWS, inputData.shadowMask);
				half4 color;
				MixRealtimeAndBakedGI(mainLight, inputData.normalWS, inputData.bakedGI, inputData.shadowMask);
				color.rgb = GlobalIllumination(brdfData, inputData.bakedGI, Occlusion, inputData.positionWS, inputData.normalWS, inputData.viewDirectionWS);
				color.a = Alpha;
				
				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif
				
				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				#ifdef ASE_DEPTH_WRITE_ON
					outputDepth = DepthValue;
				#endif
				
				return BRDFDataToGbuffer(brdfData, inputData, Smoothness, Emission + color.rgb);
			}

			ENDHLSL
		}

	
	}
	
	CustomEditor "ASEMaterialInspector"
	Fallback "Hidden/InternalErrorShader"
	
}
/*ASEBEGIN
Version=18935
0;0;2560;1379;3785.589;1291.678;2.358843;True;True
Node;AmplifyShaderEditor.CommentaryNode;145;-4798.475,4455.982;Inherit;False;1698.65;529.3062;FurWind;15;136;142;141;139;133;140;128;130;129;132;131;143;144;146;148;FurWind;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;142;-4748.475,4869.289;Inherit;False;Constant;_Float3;Float 3;25;0;Create;True;0;0;0;False;0;False;0.01;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;141;-4745.475,4720.289;Inherit;False;Property;_FurWind;FurWind;25;0;Create;True;0;0;0;False;0;False;0,0;1,1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.SimpleTimeNode;136;-4735.475,4628.289;Inherit;False;1;0;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;50;-2757.253,2411.969;Inherit;False;1829.936;848.3505;Normal;13;49;74;75;72;73;69;68;67;66;64;70;18;48;Normal;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;48;-2677.777,2531.645;Inherit;False;Property;_Normal;Normal;6;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;67;-2646.655,2692.146;Inherit;True;Property;_HeightMap;HeightMap;8;0;Create;True;0;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;68;-2609.655,3045.146;Inherit;False;Property;_HeightOffset;HeightOffset;9;0;Create;True;0;0;0;False;0;False;1;0.25;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;66;-2633.655,2912.146;Inherit;False;0;67;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;69;-2584.655,3131.146;Inherit;False;Property;_Height;Height;10;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;133;-4675.879,4505.982;Inherit;False;0;128;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;139;-4525.475,4670.289;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;140;-4393.365,4568.614;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.FunctionNode;64;-2325.885,2729.234;Inherit;False;Normal From Texture(SG);-1;;4;75f768fcf37fcdb46b862c154348de82;0;4;174;SAMPLER2D;0;False;176;FLOAT2;0,0;False;177;FLOAT;0;False;178;FLOAT;0;False;2;FLOAT3;40;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;18;-2470.36,2464.305;Inherit;True;Property;_NormalMap;NormalMap;5;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.BlendNormalsNode;70;-2032.589,2541.13;Inherit;False;0;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;128;-4243.277,4529.808;Inherit;True;Property;_FlowMap;FlowMap;24;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;148;-3804.487,4618.097;Inherit;False;Constant;_Float5;Float 5;27;0;Create;True;0;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.NegateNode;73;-1863.508,2713.044;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.VertexColorNode;103;-923.6221,825.1539;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SwizzleNode;130;-3934.665,4535.326;Inherit;False;FLOAT2;0;1;2;3;1;0;COLOR;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;75;-1742.508,2854.044;Inherit;False;Property;_SwitchNormalbyFace;Switch Normal by Face;7;1;[Toggle];Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;150;-3936.59,3975.865;Inherit;False;466.4292;308.3303;Comment;3;123;126;125;;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;104;-731.6221,849.1539;Inherit;False;Fur_Layer;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SwitchByFaceNode;72;-1712.508,2623.044;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;132;-3638.859,4630.972;Inherit;False;Constant;_Float2;Float 2;25;0;Create;True;0;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;129;-3650.017,4533.456;Inherit;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;2;False;1;FLOAT2;0
Node;AmplifyShaderEditor.CommentaryNode;42;-2783.893,1610.908;Inherit;False;1211.842;737.5865;Roughness;9;40;39;36;32;17;43;44;46;45;Roughness;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;131;-3498.966,4578.83;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;143;-3473.284,4684.502;Inherit;False;Property;_FurWindInt;FurWindInt;26;0;Create;True;0;0;0;False;0;False;0;0.3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;74;-1484.49,2547.604;Inherit;False;3;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;146;-3495.653,4851.469;Inherit;False;104;Fur_Layer;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;126;-3886.59,4168.196;Inherit;False;104;Fur_Layer;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;62;-3412.496,3436.673;Inherit;False;2646.755;1410.818;Alpha;22;60;116;57;115;58;59;118;117;113;56;54;122;121;119;110;53;55;114;120;111;124;147;Alpha;1,1,1,1;0;0
Node;AmplifyShaderEditor.Vector2Node;123;-3882.447,4025.865;Inherit;False;Property;_FurDirection;FurDirection;23;0;Create;True;0;0;0;False;0;False;0,0;0,0.05;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RegisterLocalVarNode;49;-1284.657,2543.746;Inherit;False;NoramlTS;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;125;-3669.269,4066.583;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;111;-3260.311,3882.201;Inherit;False;0;110;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;144;-3255.323,4583.628;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;43;-2726.41,2082.795;Inherit;False;Property;_SheenRoughness;SheenRoughness;14;0;Create;True;0;0;0;False;0;False;0;0.31;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;17;-2733.893,1660.908;Inherit;True;Property;_RoughnessMap;RoughnessMap;3;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;32;-2739.673,1923.152;Inherit;False;Property;_Roughness;Roughness;4;0;Create;True;0;0;0;False;0;False;0;0.81;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;89;-1520.716,1299.478;Inherit;False;1916.892;1027.164;SheenDFG;13;88;86;77;78;85;83;82;81;79;98;99;100;101;SheenDFG;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;44;-2314.729,2010.424;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;124;-2914.561,4043.046;Inherit;False;3;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT2;0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;36;-2341.973,1769.958;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;78;-1470.716,1349.478;Inherit;False;49;NoramlTS;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;114;-2643.593,4405.214;Inherit;False;104;Fur_Layer;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;147;-2424.471,4107.114;Inherit;False;Constant;_Float4;Float 4;27;0;Create;True;0;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;82;-1207.03,1538.483;Inherit;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ClampOpNode;39;-2185.98,1763.605;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0.001;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;153;-3900.687,711.9313;Inherit;False;104;Fur_Layer;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;120;-2686.198,4311.51;Inherit;False;Property;_EdgeFade;EdgeFade;22;0;Create;True;0;0;0;False;0;False;0;1;0;3;0;1;FLOAT;0
Node;AmplifyShaderEditor.WorldNormalVector;79;-1225.624,1352.552;Inherit;False;False;1;0;FLOAT3;0,0,1;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RangedFloatNode;155;-3954.586,596.001;Inherit;False;Property;_FurOcclusion;FurOcclusion;27;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;110;-2715.148,3978.345;Inherit;True;Property;_FurPattern;FurPattern;21;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ClampOpNode;45;-2115.73,1987.424;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0.001;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;53;-2715.557,3486.673;Inherit;True;Property;_AlphaMap;AlphaMap;15;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;MipLevel;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;55;-2587.545,3752.417;Inherit;False;Property;_Alpha;Alpha;16;0;Create;True;0;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;37;-2769.463,-129.0522;Inherit;False;1371.538;710.5144;SheenColor;6;96;93;92;34;94;158;SheenColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.LerpOp;154;-3643.586,601.001;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;46;-1944.613,1961.424;Inherit;False;SheenRoughness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;119;-2345.127,4319.702;Inherit;False;3;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;40;-1960.217,1773.142;Inherit;False;Roughness;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;81;-1003.54,1374.244;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;54;-2396.472,3550.205;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;121;-2264.744,4032.283;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;122;-2096.552,4293.104;Inherit;False;104;Fur_Layer;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;96;-2624.852,10.40204;Inherit;False;0;93;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ClampOpNode;83;-845.497,1386.64;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;113;-2062.932,4027.466;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;156;-3406.783,605.087;Inherit;False;FurOcc;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;86;-871.7047,1573.17;Inherit;False;46;SheenRoughness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;117;-1870.569,4291.448;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.01;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;31;-2646.037,-702.753;Inherit;False;971.8608;498.1647;BaseColor;5;16;27;28;29;160;BaseColor;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;98;-808.5578,1864.549;Inherit;False;40;Roughness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;56;-2176.185,3549.472;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;158;-2346.572,371.584;Inherit;False;156;FurOcc;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;16;-2596.037,-652.753;Inherit;True;Property;_BaseMap;BaseMap;1;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;28;-2467.696,-416.5883;Inherit;False;Property;_BaseColor;BaseColor;2;0;Create;True;0;0;0;False;0;False;1,1,1,0;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DitheringNode;58;-1987.753,3640.964;Inherit;False;0;False;4;0;FLOAT;0;False;1;SAMPLER2D;;False;2;FLOAT4;0,0,0,0;False;3;SAMPLERSTATE;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;93;-2406.589,-39.78178;Inherit;True;Property;_SheenMap;SheenMap;12;0;Create;True;0;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;85;-662.2028,1434.376;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;59;-2047.235,3784.598;Inherit;False;Property;_Dither;Dither;18;0;Create;True;0;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;118;-1886.258,4026.452;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;1;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;92;-2488.795,169.8895;Inherit;False;Property;_SheenColor;SheenColor;13;0;Create;True;0;0;0;False;0;False;1,1,1,0;0.2735849,0.2735849,0.2735849,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;160;-2168.377,-363.3911;Inherit;False;156;FurOcc;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;99;-540.2695,1790.689;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;27;-2122.976,-555.9692;Inherit;False;3;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;57;-1735.696,3539.381;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ClampOpNode;115;-1724.196,4031.087;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;77;-449.1444,1426.282;Inherit;True;Property;_ClothDFG_LUT;ClothDFG_LUT;11;0;Create;True;0;0;0;False;0;False;-1;48bf7583e2a97074ab6e3ca3dadff057;48bf7583e2a97074ab6e3ca3dadff057;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;100;-376.765,1706.008;Inherit;True;Property;_ClothDFG_LUT1;ClothDFG_LUT;11;0;Create;True;0;0;0;False;0;False;-1;48bf7583e2a97074ab6e3ca3dadff057;48bf7583e2a97074ab6e3ca3dadff057;True;0;False;white;Auto;False;Instance;77;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;94;-2086.377,143.4334;Inherit;False;3;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;29;-1898.176,-652.4433;Inherit;False;BaseColor;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;34;-1877.748,0.9478073;Inherit;False;SheenColor;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;116;-1524.73,3951.391;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;101;62.19567,1691.677;Inherit;False;ClothDFG;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;88;-30.05189,1441.287;Inherit;False;SheenDFG;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;107;-703.6221,956.1539;Inherit;False;Property;_FurLengthm;FurLengthm;20;0;Create;True;0;0;0;False;0;False;0.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;90;-1237.486,178.3071;Inherit;False;88;SheenDFG;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;47;-1236.74,116.266;Inherit;False;46;SheenRoughness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;60;-1164.178,3712.836;Inherit;False;Alpha;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;30;-1220.984,-161.7458;Inherit;False;29;BaseColor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.NormalVertexDataNode;105;-564.6221,646.1539;Inherit;False;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;102;-1228.238,248.2839;Inherit;False;101;ClothDFG;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;109;-502.6222,797.1539;Inherit;False;Constant;_Float1;Float 1;20;0;Create;True;0;0;0;False;0;False;0.01;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;108;-493.6222,905.1539;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;35;-1226.842,-65.86268;Inherit;False;34;SheenColor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;41;-1237.103,20.75432;Inherit;False;40;Roughness;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;52;-1340.183,414.5253;Inherit;False;GetSSAO;-1;;5;602825c37d726b54bad85a2f60ad872a;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;76;-1130.417,750.9482;Inherit;False;Property;_EnvRotation;EnvRotation;19;0;Create;True;0;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;51;-1199.87,317.9713;Inherit;False;49;NoramlTS;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;106;-316.6222,740.1539;Inherit;False;3;3;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;12;-320,-4.5;Inherit;False;Constant;_Float0;Float 0;0;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;127;-485.6012,13.38986;Inherit;False;49;NoramlTS;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;26;-880.8483,66.33743;Inherit;False;ASE_ClothMaster;-1;;6;1125c4c94a7f294409db126de18d2f89;0;9;6;FLOAT3;0,0,0;False;21;FLOAT3;0,0,0;False;10;FLOAT;0;False;22;FLOAT;0;False;24;FLOAT;0;False;23;FLOAT;0;False;9;FLOAT3;0,0,0;False;19;FLOAT;0;False;20;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;63;-296.0134,494.3926;Inherit;False;Property;_AlphaClip;AlphaClip;17;0;Create;True;0;0;0;False;0;False;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;61;-364.7581,351.5382;Inherit;False;60;Alpha;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;71;-2736.311,-1523.615;Inherit;False;Property;_CullMode;CullMode;0;1;[Enum];Create;True;0;0;1;UnityEngine.Rendering.CullMode;True;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;0,0;Float;False;True;-1;2;ASEMaterialInspector;0;2;BRDF/Cloth_Fur;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;19;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;0;True;71;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;7;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;1;LightMode=UniversalForward;False;False;0;Hidden/InternalErrorShader;0;0;Standard;40;Workflow;0;637890899445308047;Surface;0;0;  Refraction Model;0;0;  Blend;0;0;Two Sided;1;0;Fragment Normal Space,InvertActionOnDeselection;0;0;Transmission;0;0;  Transmission Shadow;0.5,False,-1;0;Translucency;0;0;  Translucency Strength;1,False,-1;0;  Normal Distortion;0.5,False,-1;0;  Scattering;2,False,-1;0;  Direct;0.9,False,-1;0;  Ambient;0.1,False,-1;0;  Shadow;0.5,False,-1;0;Cast Shadows;1;0;  Use Shadow Threshold;0;0;Receive Shadows;1;0;GPU Instancing;1;0;LOD CrossFade;1;0;Built-in Fog;1;0;_FinalColorxAlpha;0;0;Meta Pass;1;0;Override Baked GI;0;0;Extra Pre Pass;0;0;DOTS Instancing;0;0;Tessellation;0;0;  Phong;0;0;  Strength;0.5,False,-1;0;  Type;0;0;  Tess;16,False,-1;0;  Min;10,False,-1;0;  Max;25,False,-1;0;  Edge Length;16,False,-1;0;  Max Displacement;25,False,-1;0;Write Depth;0;0;  Early Z;0;0;Vertex Position,InvertActionOnDeselection;1;0;Debug Display;0;0;Clear Coat;0;0;0;10;False;True;True;True;True;False;True;True;False;False;False;;True;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;True;False;False;False;False;0;False;-1;False;False;False;False;False;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthNormals;0;6;DepthNormals;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=DepthNormals;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;9;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ScenePickingPass;0;9;ScenePickingPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Picking;False;True;4;d3d11;glcore;gles;gles3;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;7;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;GBuffer;0;7;GBuffer;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;False;False;False;True;1;LightMode=UniversalGBuffer;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=Universal2D;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;8;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;SceneSelectionPass;0;8;SceneSelectionPass;0;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=SceneSelectionPass;False;True;4;d3d11;glcore;gles;gles3;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraphLitGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;False;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;True;17;d3d9;d3d11;glcore;gles;gles3;metal;vulkan;xbox360;xboxone;xboxseries;ps4;playstation;psp2;n3ds;wiiu;switch;nomrt;0;False;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;139;0;136;0
WireConnection;139;1;141;0
WireConnection;139;2;142;0
WireConnection;140;0;133;0
WireConnection;140;1;139;0
WireConnection;64;174;67;0
WireConnection;64;176;66;0
WireConnection;64;177;68;0
WireConnection;64;178;69;0
WireConnection;18;5;48;0
WireConnection;70;0;18;0
WireConnection;70;1;64;40
WireConnection;128;1;140;0
WireConnection;73;0;70;0
WireConnection;130;0;128;0
WireConnection;104;0;103;1
WireConnection;72;0;70;0
WireConnection;72;1;73;0
WireConnection;129;0;130;0
WireConnection;129;1;148;0
WireConnection;131;0;129;0
WireConnection;131;1;132;0
WireConnection;74;0;70;0
WireConnection;74;1;72;0
WireConnection;74;2;75;0
WireConnection;49;0;74;0
WireConnection;125;0;123;0
WireConnection;125;1;126;0
WireConnection;125;2;126;0
WireConnection;144;0;131;0
WireConnection;144;1;143;0
WireConnection;144;2;146;0
WireConnection;44;0;17;1
WireConnection;44;1;43;0
WireConnection;124;0;111;0
WireConnection;124;1;125;0
WireConnection;124;2;144;0
WireConnection;36;0;17;1
WireConnection;36;1;32;0
WireConnection;39;0;36;0
WireConnection;79;0;78;0
WireConnection;110;1;124;0
WireConnection;45;0;44;0
WireConnection;154;0;155;0
WireConnection;154;2;153;0
WireConnection;46;0;45;0
WireConnection;119;0;120;0
WireConnection;119;1;114;0
WireConnection;119;2;114;0
WireConnection;40;0;39;0
WireConnection;81;0;79;0
WireConnection;81;1;82;0
WireConnection;54;0;53;1
WireConnection;54;1;55;0
WireConnection;121;0;110;1
WireConnection;121;1;147;0
WireConnection;83;0;81;0
WireConnection;113;0;121;0
WireConnection;113;1;119;0
WireConnection;156;0;154;0
WireConnection;117;0;122;0
WireConnection;56;0;54;0
WireConnection;58;0;56;0
WireConnection;93;1;96;0
WireConnection;85;0;83;0
WireConnection;85;1;86;0
WireConnection;118;0;113;0
WireConnection;118;2;117;0
WireConnection;99;0;83;0
WireConnection;99;1;98;0
WireConnection;27;0;16;0
WireConnection;27;1;28;0
WireConnection;27;2;160;0
WireConnection;57;0;56;0
WireConnection;57;1;58;0
WireConnection;57;2;59;0
WireConnection;115;0;118;0
WireConnection;77;1;85;0
WireConnection;100;1;99;0
WireConnection;94;0;93;0
WireConnection;94;1;92;0
WireConnection;94;2;158;0
WireConnection;29;0;27;0
WireConnection;34;0;94;0
WireConnection;116;0;57;0
WireConnection;116;1;115;0
WireConnection;101;0;100;1
WireConnection;88;0;77;1
WireConnection;60;0;116;0
WireConnection;108;0;104;0
WireConnection;108;1;107;0
WireConnection;106;0;105;0
WireConnection;106;1;109;0
WireConnection;106;2;108;0
WireConnection;26;6;30;0
WireConnection;26;21;35;0
WireConnection;26;10;41;0
WireConnection;26;22;47;0
WireConnection;26;24;90;0
WireConnection;26;23;102;0
WireConnection;26;9;51;0
WireConnection;26;19;52;0
WireConnection;26;20;76;0
WireConnection;1;0;12;0
WireConnection;1;1;127;0
WireConnection;1;2;26;0
WireConnection;1;9;12;0
WireConnection;1;4;12;0
WireConnection;1;5;12;0
WireConnection;1;6;61;0
WireConnection;1;7;63;0
WireConnection;1;8;106;0
ASEEND*/
//CHKSM=87532FCBECADC24BC8DF8FA7CD4EDE28EB069C47