#pragma once
#include "./HairDataYaotai.hlsl"
#include "./LightingYaotai.hlsl"

CBUFFER_START(UnityPerMaterial)
float4 _DiffuseColor;
float4 _DiffuseColor2;
float4 _FirstSpecularColor;
float4 _SecondSpecularColor;
float _FirstStrength;
float _FirstWidth;
float _FirstOffset;
float _SecondStrength;
float _SecondWidth;
float _SecondOffset;
float _ClipValue;
float _ClipValue2;
float _ClipValue3;
float _Anisotropy;
float _NormalIntensity;
float4 _MainColor_ST;
float4 _NormalMap_ST;
float4 _AnisotropyTexture_ST;
float3 _LightOffset;
float4 _FaweiAlpha;
CBUFFER_END


TEXTURE2D(_MainColor);    
SAMPLER(sampler_MainColor);

TEXTURE2D(_NormalMap);
SAMPLER(sampler_NormalMap);

TEXTURE2D(_AnisotropyTexture);
SAMPLER(sampler_AnisotropyTexture);

TEXTURE2D(_FlowMap);
SAMPLER(sampler_FlowMap);


TEXTURE2D(_Mask2);
SAMPLER(sampler_Mask2);

TEXTURE2D(_AlhphMap);
SAMPLER(sampler_AlhphMap);

TEXTURE2D(_DirNoise);
SAMPLER(sampler_DirNoise);

VertexOutput Vertex(VertexInput v)
{
    VertexOutput o;

    UNITY_SETUP_INSTANCE_ID(v);
    UNITY_TRANSFER_INSTANCE_ID(v, o);
    UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

    o.pos = TransformObjectToHClip(v.vertex.xyz);
    
    o.uv_MainTex = v.texcoord.xy * _MainColor_ST.xy + _MainColor_ST.zw;
    o.uv_MainTex2 = v.texcoord1.xy * _MainColor_ST.xy + _MainColor_ST.zw;
    o.uv_MainTex3 = v.texcoord2.xy * _MainColor_ST.xy + _MainColor_ST.zw;  
    o.uv_NormalTex = v.texcoord.xy * _NormalMap_ST.xy + _NormalMap_ST.zw;

    float3 worldPos = TransformObjectToWorld(v.vertex.xyz);

    VertexNormalInputs normalInput = GetVertexNormalInputs(v.normal.xyz, v.tangent);

    o.worldNormalDir = float4(normalInput.normalWS, worldPos.x);
    o.worldTangentDir = float4(normalInput.tangentWS, worldPos.y);
    o.worldBitangentDir = float4(normalInput.bitangentWS, worldPos.z);
    o.worldPos = worldPos;

    return o;
}


float3 CalculateAdditionalLight(VertexOutput vertexOutput, float4 _DiffuseColor, float3 normal, float3 basicColor)
{
    float3 finalColor;
    float3 Diffuse;
    float3 Specular;

    int lightCount = GetAdditionalLightsCount();
    for(int lightIndex = 0; lightIndex < lightCount; lightIndex++)
    {
        Light light = GetAdditionalLight(lightIndex, vertexOutput.worldPos);
        CusLightingData lightingData = CalculateLightingData(vertexOutput, normal, light.direction);

        Diffuse = max(0, 0.75 * lightingData.NoL + 0.25) * lerp(_DiffuseColor.rgb,_DiffuseColor2.rgb,basicColor.r);

        float shift = SAMPLE_TEXTURE2D(_AnisotropyTexture, sampler_AnisotropyTexture, vertexOutput.uv_MainTex).r * 2 - 1;
        float3 shiftedTangent1 = lerp(vertexOutput.worldBitangentDir.xyz + _FirstOffset , ShiftTangent(vertexOutput, shift + _FirstOffset), _Anisotropy);
        float3 shiftedTangent2 = lerp(vertexOutput.worldBitangentDir.xyz + _SecondOffset, ShiftTangent(vertexOutput, shift + _SecondOffset), _Anisotropy);

        float3 FirstSpecular = _FirstSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _FirstWidth, _FirstStrength, shiftedTangent1);
        float3 SecondSpecular = _SecondSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _SecondWidth, _SecondStrength, shiftedTangent2);

        float clampedNdotV = max(lightingData.NoV, 0.0001);
        float clampedNdotL = saturate(lightingData.NoL);
        Specular = (FirstSpecular + SecondSpecular) * saturate(lightingData.NoL * lightingData.NoV) * clampedNdotL;

        finalColor += (Specular / lightCount + Diffuse / lightCount) * light.color;
    }

    return finalColor;
}

float4 FragCutOff(VertexOutput vertexOutput) : SV_Target
{
    float4 basicColor = SAMPLE_TEXTURE2D(_MainColor, sampler_MainColor, vertexOutput.uv_MainTex);
    float4 mask2 = SAMPLE_TEXTURE2D(_Mask2, sampler_Mask2, vertexOutput.uv_MainTex);
    float4 alphaColor = SAMPLE_TEXTURE2D(_AlhphMap, sampler_AlhphMap, vertexOutput.uv_MainTex);
    float alpha = basicColor.a * mask2.a * lerp(1,mask2.b,alphaColor.r) + basicColor.g * basicColor.r;   

    //剔除表面头发
    float uvx = vertexOutput.uv_MainTex.x;
    uvx /= 6.0f;
    uvx = saturate(uvx);
    uvx = step(0.061,uvx);
    //return uvx.rrrr;

    if(alpha - _ClipValue < 0 || (1 - basicColor.a < _ClipValue2) || saturate(alphaColor.r * 10 * mask2.g) > _ClipValue3 || uvx > 0.25f)
    {
        discard;
    }

    float3 normal = UnpackNormal(SAMPLE_TEXTURE2D(_NormalMap, sampler_NormalMap, vertexOutput.uv_NormalTex));
    normal = float3(normal.rg * _NormalIntensity , lerp(1,normal.b,saturate(_NormalIntensity)));
    Light mainLight = GetMainLight();

    CusLightingData lightingData = CalculateLightingData(vertexOutput, normal, normalize(mainLight.direction + _LightOffset));

    float3 Diffuse = DiffuseTerm(lightingData) * _DiffuseColor.rgb * (mask2.a);
    float shift = SAMPLE_TEXTURE2D(_AnisotropyTexture, sampler_AnisotropyTexture, vertexOutput.uv_MainTex * _AnisotropyTexture_ST.xy).r * 2 - 1;
    float3 shiftedTangent1 = lerp(vertexOutput.worldBitangentDir.xyz + _FirstOffset , ShiftTangent(vertexOutput, shift + _FirstOffset), _Anisotropy);
    float3 shiftedTangent2 = lerp(vertexOutput.worldBitangentDir.xyz + _SecondOffset, ShiftTangent(vertexOutput, shift + _SecondOffset), _Anisotropy);
    
    float3 FirstSpecular = _FirstSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _FirstWidth, _FirstStrength, shiftedTangent1);
    float3 SecondSpecular = _SecondSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _SecondWidth, _SecondStrength, shiftedTangent2);
    

    float clampedNdotV = max(lightingData.NoV, 0.0001);
    float clampedNdotL = saturate(lightingData.NoL);
    float3 Specular = (FirstSpecular + SecondSpecular) * saturate(lightingData.NoL * lightingData.NoV) * clampedNdotL * mask2.a;

    float3 finalColor;

    #ifdef _EnviromentLighting

        finalColor = (Diffuse + Specular) * mainLight.color + SampleSH(lightingData.worldNormal) * 0.02;

    #else

        finalColor = (Diffuse + Specular) * mainLight.color;

    #endif

    #ifdef _AdditionalLights

        float3 additionalColor = CalculateAdditionalLight(vertexOutput, _DiffuseColor, normal, basicColor.rgb);
        finalColor += additionalColor;

    #endif

    return float4(finalColor, 1);
}

float4 FragAlphaBlend(VertexOutput vertexOutput) : SV_Target
{
    float4 hairColor = SAMPLE_TEXTURE2D(_AlhphMap, sampler_AlhphMap, vertexOutput.uv_MainTex);
    float4 basicColor = SAMPLE_TEXTURE2D(_MainColor, sampler_MainColor, vertexOutput.uv_MainTex);
    float4 mask2 = SAMPLE_TEXTURE2D(_Mask2, sampler_Mask2, vertexOutput.uv_MainTex);
    float alpha = mask2.b;     
    alpha = saturate(mask2.a * mask2.r * 0.5 + mask2.a * basicColor.a * lerp(1,5,mask2.b) + basicColor.g);

    float4 fawei_mask = SAMPLE_TEXTURE2D(_DirNoise, sampler_DirNoise, vertexOutput.uv_MainTex * float2(3,1));

    float fawei_alpha = saturate(pow(abs(saturate(1 - basicColor.a) * _FaweiAlpha.x),_FaweiAlpha.y));
    fawei_alpha = mask2.a - fawei_alpha;
    fawei_alpha = saturate(fawei_alpha * 2);
    alpha *= fawei_alpha;             //后脑发根部位透明模拟
    alpha *= mask2.g * 15;            //透明区域拉丝模拟
    alpha = saturate(alpha);
    alpha = saturate(alpha - hairColor.r * 1.5);

    float4 tempGradient = SAMPLE_TEXTURE2D(_MainColor, sampler_MainColor, vertexOutput.uv_MainTex2 + float2(0,0.5));
    alpha *= tempGradient.a;       //尾部麻花透明


   //删除侧边自遮挡的部分
    float uvx3 = vertexOutput.uv_MainTex3.x;
    uvx3 = step(1,uvx3);
    float tempStepVal = step(1.31,vertexOutput.uv_MainTex3.x);    //上层头发遮罩
    uvx3 -= tempStepVal;
   alpha -= uvx3;
   alpha = saturate(alpha);
   
   alpha = lerp(alpha,saturate(fawei_mask.r * 1.4 ),tempStepVal * mask2.a);

   float tt_mask = tempStepVal * mask2.a;


    float3 normal = UnpackNormal(SAMPLE_TEXTURE2D(_NormalMap, sampler_NormalMap, vertexOutput.uv_NormalTex));
    normal = float3(normal.rg * _NormalIntensity , lerp(1,normal.b,saturate(_NormalIntensity)));
    Light mainLight = GetMainLight();

    CusLightingData lightingData = CalculateLightingData(vertexOutput, normal, normalize(mainLight.direction + _LightOffset));

    float3 flowmap = SAMPLE_TEXTURE2D(_FlowMap, sampler_FlowMap, vertexOutput.uv_MainTex).rgb;
    flowmap = float3(flowmap.xy,0);
    flowmap = flowmap * 2 - 1;
    flowmap = normalize(flowmap);
	float3 TtoW1 = float3(vertexOutput.worldTangentDir.x,vertexOutput.worldBitangentDir.x,vertexOutput.worldNormalDir.x);
    float3 TtoW2 = float3(vertexOutput.worldTangentDir.y,vertexOutput.worldBitangentDir.y,vertexOutput.worldNormalDir.y);
    float3 TtoW3 = float3(vertexOutput.worldTangentDir.z,vertexOutput.worldBitangentDir.z,vertexOutput.worldNormalDir.z);

	float3x3 TtoW_Matrix = float3x3(TtoW1, TtoW2, TtoW3);
    flowmap = mul(TtoW_Matrix,flowmap);

    float3 Diffuse = DiffuseTerm(lightingData) * _DiffuseColor.rgb* (mask2.a);
    float shift = SAMPLE_TEXTURE2D(_AnisotropyTexture, sampler_AnisotropyTexture, vertexOutput.uv_MainTex * _AnisotropyTexture_ST.xy).r * 2 - 1;
    float3 shiftedTangent1 = lerp( vertexOutput.worldBitangentDir.xyz + _FirstOffset , ShiftTangent(vertexOutput, shift + _FirstOffset), _Anisotropy);
    float3 shiftedTangent2 = lerp( vertexOutput.worldBitangentDir.xyz + _SecondOffset, ShiftTangent(vertexOutput, shift + _SecondOffset), _Anisotropy);

    float3 FirstSpecular = _FirstSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _FirstWidth, _FirstStrength, shiftedTangent1);
    float3 SecondSpecular = _SecondSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _SecondWidth, _SecondStrength, shiftedTangent2);

    float clampedNdotV = max(lightingData.NoV, 0.0001);
    float clampedNdotL = saturate(lightingData.NoL);
    float3 Specular = (FirstSpecular + SecondSpecular) * saturate(lightingData.NoL * lightingData.NoV) * clampedNdotL * mask2.a;

    float3 finalColor;

    #ifdef _EnviromentLighting

        finalColor = (Diffuse + Specular) * mainLight.color + SampleSH(lightingData.worldNormal) * 0.02;

    #else

        finalColor = (Diffuse + Specular) * mainLight.color;

    #endif

    #ifdef _AdditionalLights

        float3 additionalColor = CalculateAdditionalLight(vertexOutput, _DiffuseColor, normal, basicColor.rgb);
        finalColor += additionalColor;

    #endif

    return float4(finalColor, alpha );
}
