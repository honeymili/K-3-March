#pragma once
#include "./HairData.hlsl"
#include "./Lighting.hlsl"

CBUFFER_START(UnityPerMaterial)
float4 _DiffuseColor;
float4 _FirstSpecularColor;
float4 _SecondSpecularColor;
float _FirstStrength;
float _FirstWidth;
float _FirstOffset;
float _SecondStrength;
float _SecondWidth;
float _SecondOffset;
float _ClipValue;
float _Anisotropy;
float _NormalIntensity;
float4 _MainColor_ST;
float4 _NormalMap_ST;
float4 _AnisotropyTexture_ST;
float3 _LightOffset;
float _ClipValue2;

float _AlphaIntensity;
float4 _Clip2MulPow;
CBUFFER_END


TEXTURE2D(_MainColor);    
SAMPLER(sampler_MainColor);

TEXTURE2D(_NormalMap);
SAMPLER(sampler_NormalMap);

TEXTURE2D(_AnisotropyTexture);
SAMPLER(sampler_AnisotropyTexture);

TEXTURE2D(_RandomTex);
SAMPLER(sampler_RandomTex);


VertexOutput Vertex(VertexInput v)
{
    VertexOutput o;

    UNITY_SETUP_INSTANCE_ID(v);
    UNITY_TRANSFER_INSTANCE_ID(v, o);
    UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

    o.pos = TransformObjectToHClip(v.vertex.xyz);
    
    o.uv_MainTex = v.texcoord.xy * _MainColor_ST.xy + _MainColor_ST.zw;
    o.uv_MainTex2 = v.texcoord1.xy * _MainColor_ST.xy + _MainColor_ST.zw;
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

        Diffuse = max(0, 0.75 * lightingData.NoL + 0.25) * _DiffuseColor.rgb;

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
    float mask2 = SAMPLE_TEXTURE2D(_RandomTex, sampler_RandomTex, vertexOutput.uv_MainTex).r;

    float alpha = basicColor.a;

    //女性头发刘海遮罩
    float liuhai_mask = vertexOutput.uv_MainTex2.x;
    liuhai_mask = step(1.01f,liuhai_mask);

    //刘海灰度渐变
    float liuhai_gradient = saturate(pow(abs(vertexOutput.uv_MainTex2.y * _Clip2MulPow.x),_Clip2MulPow.y));
    
    alpha = lerp(alpha,alpha * liuhai_gradient,liuhai_mask);
    float clipVal = lerp(_ClipValue,_ClipValue2,liuhai_mask);

    if(alpha - clipVal < 0 )
    {
        discard;
    }

    float3 normal = UnpackNormal(SAMPLE_TEXTURE2D(_NormalMap, sampler_NormalMap, vertexOutput.uv_NormalTex));
    normal = float3(normal.rg * _NormalIntensity , lerp(1,normal.b,saturate(_NormalIntensity)));

    Light mainLight = GetMainLight();

    CusLightingData lightingData = CalculateLightingData(vertexOutput, normal, normalize(mainLight.direction + _LightOffset));

    float3 Diffuse = DiffuseTerm(lightingData) * _DiffuseColor.rgb * (mask2);
    float shift = SAMPLE_TEXTURE2D(_AnisotropyTexture, sampler_AnisotropyTexture, vertexOutput.uv_MainTex * _AnisotropyTexture_ST.xy).r * 2 - 1;
    float3 shiftedTangent1 = lerp(vertexOutput.worldBitangentDir.xyz + _FirstOffset , ShiftTangent(vertexOutput, shift + _FirstOffset), _Anisotropy);
    float3 shiftedTangent2 = lerp(vertexOutput.worldBitangentDir.xyz + _SecondOffset, ShiftTangent(vertexOutput, shift + _SecondOffset), _Anisotropy);
    

    float3 FirstSpecular = _FirstSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _FirstWidth, _FirstStrength, shiftedTangent1);
    float3 SecondSpecular = _SecondSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _SecondWidth, _SecondStrength, shiftedTangent2);
    

    float clampedNdotV = max(lightingData.NoV, 0.0001);
    float clampedNdotL = saturate(lightingData.NoL);
    float3 Specular = (FirstSpecular + SecondSpecular) * saturate(lightingData.NoL * lightingData.NoV) * clampedNdotL * mask2;

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
    float4 basicColor = SAMPLE_TEXTURE2D(_MainColor, sampler_MainColor, vertexOutput.uv_MainTex);
    float mask2 = SAMPLE_TEXTURE2D(_RandomTex, sampler_RandomTex, vertexOutput.uv_MainTex).r;
    float alpha = basicColor.a  * _AlphaIntensity;    

    float3 normal = UnpackNormal(SAMPLE_TEXTURE2D(_NormalMap, sampler_NormalMap, vertexOutput.uv_NormalTex));
    normal = float3(normal.rg * _NormalIntensity , lerp(1,normal.b,saturate(_NormalIntensity)));

    Light mainLight = GetMainLight();

    CusLightingData lightingData = CalculateLightingData(vertexOutput, normal, normalize(mainLight.direction + _LightOffset));

    float3 Diffuse = DiffuseTerm(lightingData) * _DiffuseColor.rgb* (mask2);
    float shift = SAMPLE_TEXTURE2D(_AnisotropyTexture, sampler_AnisotropyTexture, vertexOutput.uv_MainTex * _AnisotropyTexture_ST.xy).r * 2 - 1;
    float3 shiftedTangent1 = lerp( vertexOutput.worldBitangentDir.xyz + _FirstOffset , ShiftTangent(vertexOutput, shift + _FirstOffset), _Anisotropy);
    float3 shiftedTangent2 = lerp( vertexOutput.worldBitangentDir.xyz + _SecondOffset, ShiftTangent(vertexOutput, shift + _SecondOffset), _Anisotropy);

    float3 FirstSpecular = _FirstSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _FirstWidth, _FirstStrength, shiftedTangent1);
    float3 SecondSpecular = _SecondSpecularColor.rgb * AnisotropySpecular(vertexOutput, lightingData, _SecondWidth, _SecondStrength, shiftedTangent2);

    float clampedNdotV = max(lightingData.NoV, 0.0001);
    float clampedNdotL = saturate(lightingData.NoL);
    float3 Specular = (FirstSpecular + SecondSpecular) * saturate(lightingData.NoL * lightingData.NoV) * clampedNdotL * mask2;

    float3 finalColor;

    #ifdef _EnvironmentLighting

        finalColor = (Diffuse + Specular) * mainLight.color + SampleSH(lightingData.worldNormal) * 0.02;

    #else

        finalColor = (Diffuse + Specular) * mainLight.color;

    #endif

    #ifdef _AdditionalLights

        float3 additionalColor = CalculateAdditionalLight(vertexOutput, _DiffuseColor, normal, basicColor.rgb);
        finalColor += additionalColor;

    #endif

    return float4(finalColor, saturate(alpha) );
}
