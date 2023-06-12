Shader "URP/Simple"
{
    Properties
    {
        _Basecolor("Basecolor",Color) = (1,1,1,1)
        _BaseMap("BaseMap", 2D) = "white" {}
        _CompMask("Mask(R=Metal,G=Roughness,B=AO)",2D) = "white"{}
        _NormalMap("Normal Map",2D) = "bump"{}
        _AO("AO",Float) = 0
        _EnvMap("EnvMap", Cube) = "_Skybox" {}
        _Tint("Tint",Color) = (1,1,1,1)
        _Expose("Expose",Float) = 1.0
        _Rotate("Rotate",Range(0,360)) = 0.0

        

        _RoughnessAdjust("Roughness Adjust",Range(-1,1)) = 0.0
        _MeatlAdjust("Meatl Adjust",Range(-1,1)) = 0.0
        _SpecShininess("Spec Shininess",Float)= 10
        _SpecIntensity("Spec Intensity",Float) = 10


        //SH
        [HideInInspector]custom_SHAr("Custom SHAr", Vector) = (0, 0, 0, 0)
        [HideInInspector]custom_SHAg("Custom SHAg", Vector) = (0, 0, 0, 0)
        [HideInInspector]custom_SHAb("Custom SHAb", Vector) = (0, 0, 0, 0)
        [HideInInspector]custom_SHBr("Custom SHBr", Vector) = (0, 0, 0, 0)
        [HideInInspector]custom_SHBg("Custom SHBg", Vector) = (0, 0, 0, 0)
        [HideInInspector]custom_SHBb("Custom SHBb", Vector) = (0, 0, 0, 0)
        [HideInInspector]custom_SHC("Custom SHC", Vector) = (0, 0, 0, 1)

    }
                SubShader
    {
        Tags{"RenderType" = "Opaque" "RenderPipeline" = "UniversalPipeline" "UniversalMaterialType" = "Lit" "IgnoreProjector" = "True" "ShaderModel" = "4.5"}
        LOD 300

        Pass
        {
            Name "ForwardLit"
            Tags{"LightMode" = "UniversalForward"}

            Cull[_Cull]



            HLSLPROGRAM
            #pragma exclude_renderers gles gles3 glcore
            #pragma target 4.5

            // -------------------------------------

            
            // -------------------------------------
            // Universal Pipeline keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile _ _CLUSTERED_RENDERING

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing
            #pragma instancing_options renderinglayer
            #pragma multi_compile _ DOTS_INSTANCING_ON


            #pragma vertex LitPassVertex
            #pragma fragment LitPassFragment

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
      

            struct Attritubes
            {
                float4 positionOS : POSITION;
                float3 normalOS : NORMAL;
                float4 tangentOS : TANGENT;
                float2 texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

            struct Varyings
            { 
                float2 uv : TEXCOORD0;
                float3 positionWS : TEXCOORD1;
                float3 normalWS : TEXCOORD2;
                float4 tangentWS : TEXCOORD3;
                float4 shadowCoord : TEXCOORD4;
                float4 positionCS : SV_POSITION;
                UNITY_VERTEX_INPUT_INSTANCE_ID
                //SHADOW_COORDS(5)
            };


            CBUFFER_START(UnityPerMaterial)

            TEXTURE2D(_BaseMap);	SAMPLER(sampler_BaseMap);
            TEXTURE2D(_CompMask);	SAMPLER(sampler_CompMask);
            TEXTURE2D(_NormalMap);	SAMPLER(sampler_NormalMap);

            half4 _EnvMap_HDR;
            TEXTURECUBE(_EnvMap);
            SAMPLER(sampler_EnvMap);



         
            half _AO;
            half _MeatlAdjust;
            half _RoughnessAdjust;
            half _SpecShininess;
            half _SpecIntensity;
            float4 _Basecolor;
           

            //LBL

            half4 _Tint;
            half _Expose;
            half _Rotate;

            //SH
            half4 custom_SHAr;
            half4 custom_SHAg;
            half4 custom_SHAb;
            half4 custom_SHBr;
            half4 custom_SHBg;
            half4 custom_SHBb;
            half4 custom_SHC;

            CBUFFER_END


            //球谐函数
            float3 custom_sh(float3 normal_dir)
            {
                float4 normalForSH = float4(normal_dir, 1.0);
                //SHEvalLinearL0L1
                half3 x;
                x.r = dot(custom_SHAr, normalForSH);
                x.g = dot(custom_SHAg, normalForSH);
                x.b = dot(custom_SHAb, normalForSH);

                //SHEvalLinearL2
                half3 x1, x2;
                // 4 of the quadratic (L2) polynomials
                half4 vB = normalForSH.xyzz * normalForSH.yzzx;
                x1.r = dot(custom_SHBr, vB);
                x1.g = dot(custom_SHBg, vB);
                x1.b = dot(custom_SHBb, vB);

                // Final (5th) quadratic (L2) polynomial
                half vC = normalForSH.x * normalForSH.x - normalForSH.y * normalForSH.y;
                x2 = custom_SHC.rgb * vC;

                float3 sh = max(float3(0.0, 0.0, 0.0), (x + x1 + x2));
                sh = pow(sh, 1.0 / 2.2);
                return sh;
            }

            //Cube旋转
            float3 RotateAround(float degree, float3 target)
            {
                float rad = degree * PI / 180.0f;
                float2x2 m_rotate = float2x2(cos(rad), -sin(rad),
                    sin(rad), cos(rad));
                float2 dir_rotate = mul(m_rotate, target.xz);
                target = float3(dir_rotate.x, target.y, dir_rotate.y);
                return target;
            }





            Varyings LitPassVertex(Attritubes input)
            {
                Varyings output = (Varyings)0;
                UNITY_SETUP_INSTANCE_ID(input);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);


                VertexPositionInputs vertexInput = GetVertexPositionInputs(input.positionOS.xyz);
                VertexNormalInputs normalInput = GetVertexNormalInputs(input.normalOS, input.tangentOS);
                output.positionCS = vertexInput.positionCS;
                output.positionWS = vertexInput.positionWS;
                output.normalWS = normalInput.normalWS;
                real sign = input.tangentOS.w * GetOddNegativeScale();
                half4 tangentWS = half4(normalInput.tangentWS.xyz, sign);
                output.tangentWS = tangentWS;

                output.uv = input.texcoord;
                output.shadowCoord = GetShadowCoord(vertexInput);

                return output;
            }

            half4 LitPassFragment(Varyings input) : SV_Target
            {


                UNITY_SETUP_INSTANCE_ID(input);

                //----------Textuer Info----------
                half2 UV = input.uv;
                half4 base_color = SAMPLE_TEXTURE2D(_BaseMap,sampler_BaseMap, UV)* _Basecolor;
                half4 comp_mask = SAMPLE_TEXTURE2D(_CompMask, sampler_CompMask, UV);
                half roughness = saturate(comp_mask.g + _RoughnessAdjust);
                half metal = saturate(comp_mask.r + _MeatlAdjust);
                half3 albedo_color = base_color.rgb * (1 - metal);//非金属固有颜色
                half3 spec_color = lerp(0,base_color.rgb ,metal);//金属高光颜色
                half ao = comp_mask.b + _AO;
                half3 NormalTS = UnpackNormal(SAMPLE_TEXTURE2D(_NormalMap, sampler_NormalMap, UV));

                //----------Dir----------
                float3 WorldPos = input.positionWS;
                half3 ViewDir = GetWorldSpaceNormalizeViewDir(WorldPos);
                half3 WorldNormal = normalize(input.normalWS);
                half3 WorldTangent = normalize(input.tangentWS.xyz);
                half3 WorldBinormal = normalize(cross(WorldNormal, WorldTangent) * input.tangentWS.w);
                half3x3 TBN = half3x3(WorldTangent, WorldBinormal, WorldNormal);
                WorldNormal = normalize(mul(NormalTS, TBN));                   
                half3 reflect_dir = reflect(-ViewDir, WorldNormal);
                reflect_dir = RotateAround(_Rotate, reflect_dir);
               
                //----------Light Info----------

	            float4 positionCS = TransformWorldToHClip(WorldPos);
	            float4 ShadowMask = float4(1.0,1.0,1.0,1.0);
                Light light = GetMainLight(positionCS, WorldPos, ShadowMask);
                half3 LightColor = light.color;
                half3 light_dir = light.direction;
                half atten = light.shadowAttenuation;

                //----------Direct Diffuse 直接光 漫反射----------
                
                half diff_term =  max(0.0, dot(WorldNormal, light_dir));
               // half3 direct_diffuse = albedo_color * diff_term * _LightColor0.xyz * atten * ao;
                half3 direct_diffuse = albedo_color * diff_term * LightColor.xyz * atten * ao;


                //----------Direct Specular 直接光 镜面反射----------

                half3 half_dir = normalize(light_dir + ViewDir);
                half NdotH = dot(WorldNormal, half_dir);
                half smoothness = 1.0 - roughness;
                half shininess = lerp(1, _SpecShininess, smoothness);
                half spec_term = pow(max(0.04, NdotH), shininess* smoothness);
                //half3 direct_specular = spec_term * spec_color * _LightColor0.xyz * atten* _SpecIntensity * ao;

                half3 direct_specular = spec_term * spec_color * LightColor.xyz * atten * _SpecIntensity * ao;



                //----------Indirect Diffuse 间接光 漫反射----------
                half half_lambert = (diff_term + 1) * 0.5;
                //half3 env_diffuse = custom_sh(WorldNormal) * albedo_color * half_lambert * ao;

                half3 env_diffuse = custom_sh(WorldNormal) * albedo_color * half_lambert * ao;



                //-------------Indirect specular 间接光镜 面反射----------
                roughness = roughness * (1.7 - 0.7 * roughness);
                float mip_level = roughness * 6.0;
                half4 color_cubemap = SAMPLE_TEXTURECUBE_LOD(_EnvMap, sampler_EnvMap, reflect_dir, mip_level);
                half3 env_color = DecodeHDREnvironment(color_cubemap, _EnvMap_HDR);//确保在移动端能拿到HDR信息

                //half3 env_specular = env_color * spec_color * half_lambert * _Tint.rgb * _Expose * ao;

                half3 env_specular = env_color * spec_color * half_lambert * _Tint.rgb * _Expose * ao;




                float3 final_color = direct_diffuse + direct_specular + env_diffuse*0.5 + env_specular;
                return half4(final_color, 1.0);
               
            }
                ENDHLSL
        }

    }
  
        SubShader
    {
        Tags{"RenderType" = "Opaque" "RenderPipeline" = "UniversalPipeline" "UniversalMaterialType" = "Lit" "IgnoreProjector" = "True" "ShaderModel" = "2.0"}
        LOD 300

        Pass
        {
            Name "ForwardLit"
            Tags{"LightMode" = "UniversalForward"}

            Cull[_Cull]



            HLSLPROGRAM
            #pragma only_renderers gles gles3 glcore d3d11
            //#pragma exclude_renderers gles gles3 glcore
            #pragma target 2.0

            // -------------------------------------

            
            // -------------------------------------
            // Universal Pipeline keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS _MAIN_LIGHT_SHADOWS_CASCADE _MAIN_LIGHT_SHADOWS_SCREEN
            #pragma multi_compile_fragment _ _SHADOWS_SOFT
            #pragma multi_compile _ _CLUSTERED_RENDERING

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing
            #pragma instancing_options renderinglayer
            #pragma multi_compile _ DOTS_INSTANCING_ON


            #pragma vertex LitPassVertex
            #pragma fragment LitPassFragment

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
      

            struct Attritubes
            {
                float4 positionOS : POSITION;
                float3 normalOS : NORMAL;
                float4 tangentOS : TANGENT;
                float2 texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

            struct Varyings
            { 
                float2 uv : TEXCOORD0;
                float3 positionWS : TEXCOORD1;
                float3 normalWS : TEXCOORD2;
                float4 tangentWS : TEXCOORD3;
                float4 shadowCoord : TEXCOORD4;
                float4 positionCS : SV_POSITION;
                UNITY_VERTEX_INPUT_INSTANCE_ID
                //SHADOW_COORDS(5)
            };


            CBUFFER_START(UnityPerMaterial)

            TEXTURE2D(_BaseMap);	SAMPLER(sampler_BaseMap);
            TEXTURE2D(_CompMask);	SAMPLER(sampler_CompMask);
            TEXTURE2D(_NormalMap);	SAMPLER(sampler_NormalMap);

            half4 _EnvMap_HDR;
            TEXTURECUBE(_EnvMap);
            SAMPLER(sampler_EnvMap);



         
            half _AO;
            half _MeatlAdjust;
            half _RoughnessAdjust;
            half _SpecShininess;
            half _SpecIntensity;
            float4 _Basecolor;
           

            //LBL

            half4 _Tint;
            half _Expose;
            half _Rotate;

            //SH
            half4 custom_SHAr;
            half4 custom_SHAg;
            half4 custom_SHAb;
            half4 custom_SHBr;
            half4 custom_SHBg;
            half4 custom_SHBb;
            half4 custom_SHC;

            CBUFFER_END


            //球谐函数
            float3 custom_sh(float3 normal_dir)
            {
                float4 normalForSH = float4(normal_dir, 1.0);
                //SHEvalLinearL0L1
                half3 x;
                x.r = dot(custom_SHAr, normalForSH);
                x.g = dot(custom_SHAg, normalForSH);
                x.b = dot(custom_SHAb, normalForSH);

                //SHEvalLinearL2
                half3 x1, x2;
                // 4 of the quadratic (L2) polynomials
                half4 vB = normalForSH.xyzz * normalForSH.yzzx;
                x1.r = dot(custom_SHBr, vB);
                x1.g = dot(custom_SHBg, vB);
                x1.b = dot(custom_SHBb, vB);

                // Final (5th) quadratic (L2) polynomial
                half vC = normalForSH.x * normalForSH.x - normalForSH.y * normalForSH.y;
                x2 = custom_SHC.rgb * vC;

                float3 sh = max(float3(0.0, 0.0, 0.0), (x + x1 + x2));
                sh = pow(sh, 1.0 / 2.2);
                return sh;
            }

            //Cube旋转
            float3 RotateAround(float degree, float3 target)
            {
                float rad = degree * PI / 180.0f;
                float2x2 m_rotate = float2x2(cos(rad), -sin(rad),
                    sin(rad), cos(rad));
                float2 dir_rotate = mul(m_rotate, target.xz);
                target = float3(dir_rotate.x, target.y, dir_rotate.y);
                return target;
            }





            Varyings LitPassVertex(Attritubes input)
            {
                Varyings output = (Varyings)0;
                UNITY_SETUP_INSTANCE_ID(input);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(output);


                VertexPositionInputs vertexInput = GetVertexPositionInputs(input.positionOS.xyz);
                VertexNormalInputs normalInput = GetVertexNormalInputs(input.normalOS, input.tangentOS);
                output.positionCS = vertexInput.positionCS;
                output.positionWS = vertexInput.positionWS;
                output.normalWS = normalInput.normalWS;
                real sign = input.tangentOS.w * GetOddNegativeScale();
                half4 tangentWS = half4(normalInput.tangentWS.xyz, sign);
                output.tangentWS = tangentWS;

                output.uv = input.texcoord;
                output.shadowCoord = GetShadowCoord(vertexInput);

                return output;
            }

            half4 LitPassFragment(Varyings input) : SV_Target
            {


                UNITY_SETUP_INSTANCE_ID(input);

                //----------Textuer Info----------
                half2 UV = input.uv;
                half4 base_color = SAMPLE_TEXTURE2D(_BaseMap,sampler_BaseMap, UV)* _Basecolor;
                half4 comp_mask = SAMPLE_TEXTURE2D(_CompMask, sampler_CompMask, UV);
                half roughness = saturate(comp_mask.b + _RoughnessAdjust);
                half metal = saturate(comp_mask.r + _MeatlAdjust);
                half3 albedo_color = base_color.rgb * (1 - metal);//非金属固有颜色
                half3 spec_color = lerp(0,base_color.rgb ,metal);//金属高光颜色
                half ao = comp_mask.b + _AO;
                half3 NormalTS = UnpackNormal(SAMPLE_TEXTURE2D(_NormalMap, sampler_NormalMap, UV));

                //----------Dir----------
                float3 WorldPos = input.positionWS;
                half3 ViewDir = GetWorldSpaceNormalizeViewDir(WorldPos);
                half3 WorldNormal = normalize(input.normalWS);
                half3 WorldTangent = normalize(input.tangentWS.xyz);
                half3 WorldBinormal = normalize(cross(WorldNormal, WorldTangent) * input.tangentWS.w);
                half3x3 TBN = half3x3(WorldTangent, WorldBinormal, WorldNormal);
                WorldNormal = normalize(mul(NormalTS, TBN));                   
                half3 reflect_dir = reflect(-ViewDir, WorldNormal);
                reflect_dir = RotateAround(_Rotate, reflect_dir);
               
                //----------Light Info----------

	            float4 positionCS = TransformWorldToHClip(WorldPos);
	            float4 ShadowMask = float4(1.0,1.0,1.0,1.0);
                Light light = GetMainLight(positionCS, WorldPos, ShadowMask);
                half3 LightColor = light.color;
                half3 light_dir = light.direction;
                half atten = light.shadowAttenuation;

                //----------Direct Diffuse 直接光 漫反射----------
                
                half diff_term =  max(0.0, dot(WorldNormal, light_dir));
               // half3 direct_diffuse = albedo_color * diff_term * _LightColor0.xyz * atten * ao;
                half3 direct_diffuse = albedo_color * diff_term * LightColor.xyz * atten * ao;


                //----------Direct Specular 直接光 镜面反射----------

                half3 half_dir = normalize(light_dir + ViewDir);
                half NdotH = dot(WorldNormal, half_dir);
                half smoothness = 1.0 - roughness;
                half shininess = lerp(1, _SpecShininess, smoothness);
                half spec_term = pow(max(0.04, NdotH), shininess* smoothness);
                //half3 direct_specular = spec_term * spec_color * _LightColor0.xyz * atten* _SpecIntensity * ao;

                half3 direct_specular = spec_term * spec_color * LightColor.xyz * atten * _SpecIntensity * ao;



                //----------Indirect Diffuse 间接光 漫反射----------
                half half_lambert = (diff_term + 1) * 0.5;
                //half3 env_diffuse = custom_sh(WorldNormal) * albedo_color * half_lambert * ao;

                half3 env_diffuse = custom_sh(WorldNormal) * albedo_color * half_lambert * ao;



                //-------------Indirect specular 间接光镜 面反射----------
                roughness = roughness * (1.7 - 0.7 * roughness);
                float mip_level = roughness * 6.0;
                half4 color_cubemap = SAMPLE_TEXTURECUBE_LOD(_EnvMap, sampler_EnvMap, reflect_dir, mip_level);
                half3 env_color = DecodeHDREnvironment(color_cubemap, _EnvMap_HDR);//确保在移动端能拿到HDR信息

                //half3 env_specular = env_color * spec_color * half_lambert * _Tint.rgb * _Expose * ao;

                half3 env_specular = env_color * spec_color * half_lambert * _Tint.rgb * _Expose * ao;




                float3 final_color = direct_diffuse + direct_specular + env_diffuse*0.5 + env_specular;
                return half4(final_color, 1.0);
               
            }
                ENDHLSL
        }

    }
  

}       