package org.apache.tools.ant.launch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.junit.Test;

public class LocatorDiffblueTest {
  /**
   * Test {@link Locator#getClassSource(Class)}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getClassSource(Class)}
   */
  @Test
  public void testGetClassSource_whenJavaLangObject_thenReturnNull() {
    // Arrange
    Class<Object> c = Object.class;

    // Act and Assert
    assertNull(Locator.getClassSource(c));
  }

  /**
   * Test {@link Locator#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_whenEmptyString_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(Locator.getResourceSource(new AntClassLoader(), ""));
  }

  /**
   * Test {@link Locator#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(Locator.getResourceSource(null, "Resource"));
  }

  /**
   * Test {@link Locator#getResourceSource(ClassLoader, String)}.
   * <ul>
   *   <li>When {@code Resource}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getResourceSource(ClassLoader, String)}
   */
  @Test
  public void testGetResourceSource_whenResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(Locator.getResourceSource(new AntClassLoader(), "Resource"));
  }

  /**
   * Test {@link Locator#fromURI(String)}.
   * <ul>
   *   <li>When {@code file:}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#fromURI(String)}
   */
  @Test
  public void testFromURI_whenFile_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Locator.fromURI("file:"));
  }

  /**
   * Test {@link Locator#fromURI(String)}.
   * <ul>
   *   <li>When {@code file:%}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#fromURI(String)}
   */
  @Test
  public void testFromURI_whenFile_thenReturnEmptyString2() {
    // Arrange, Act and Assert
    assertEquals("", Locator.fromURI("file:%"));
  }

  /**
   * Test {@link Locator#fromURI(String)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#fromURI(String)}
   */
  @Test
  public void testFromURI_whenUri_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> Locator.fromURI("Uri"));
  }

  /**
   * Test {@link Locator#fromJarURI(String)}.
   * <ul>
   *   <li>When {@code jar:file:!/}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#fromJarURI(String)}
   */
  @Test
  public void testFromJarURI_whenJarFile_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Locator.fromJarURI("jar:file:!/"));
  }

  /**
   * Test {@link Locator#fromJarURI(String)}.
   * <ul>
   *   <li>When {@code jar:!/}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#fromJarURI(String)}
   */
  @Test
  public void testFromJarURI_whenJar_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> Locator.fromJarURI("jar:!/"));
  }

  /**
   * Test {@link Locator#decodeUri(String)}.
   * <ul>
   *   <li>When {@code %%}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#decodeUri(String)}
   */
  @Test
  public void testDecodeUri_whenPercentSignPercentSign_thenReturnEmptyString() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals("", Locator.decodeUri("%%"));
  }

  /**
   * Test {@link Locator#decodeUri(String)}.
   * <ul>
   *   <li>When {@code %}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#decodeUri(String)}
   */
  @Test
  public void testDecodeUri_whenPercentSign_thenReturnEmptyString() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals("", Locator.decodeUri("%"));
  }

  /**
   * Test {@link Locator#decodeUri(String)}.
   * <ul>
   *   <li>When {@code %Uri}.</li>
   *   <li>Then return replacement character i.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#decodeUri(String)}
   */
  @Test
  public void testDecodeUri_whenUri_thenReturnReplacementCharacterI() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals("�i", Locator.decodeUri("%Uri"));
  }

  /**
   * Test {@link Locator#decodeUri(String)}.
   * <ul>
   *   <li>When {@code Uri}.</li>
   *   <li>Then return {@code Uri}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#decodeUri(String)}
   */
  @Test
  public void testDecodeUri_whenUri_thenReturnUri() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals("Uri", Locator.decodeUri("Uri"));
  }

  /**
   * Test {@link Locator#encodeURI(String)}.
   * <ul>
   *   <li>When {@code %Path}.</li>
   *   <li>Then return {@code %25Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#encodeURI(String)}
   */
  @Test
  public void testEncodeURI_whenPath_thenReturn25Path() {
    // Arrange, Act and Assert
    assertEquals("%25Path", Locator.encodeURI("%Path"));
  }

  /**
   * Test {@link Locator#encodeURI(String)}.
   * <ul>
   *   <li>When {@code Path}.</li>
   *   <li>Then return {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#encodeURI(String)}
   */
  @Test
  public void testEncodeURI_whenPath_thenReturnPath() {
    // Arrange, Act and Assert
    assertEquals("Path", Locator.encodeURI("Path"));
  }

  /**
   * Test {@link Locator#encodeURI(String)}.
   * <ul>
   *   <li>When {@code %%}.</li>
   *   <li>Then return {@code %25%25}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#encodeURI(String)}
   */
  @Test
  public void testEncodeURI_whenPercentSignPercentSign_thenReturn2525() {
    // Arrange, Act and Assert
    assertEquals("%25%25", Locator.encodeURI("%%"));
  }

  /**
   * Test {@link Locator#encodeURI(String)}.
   * <ul>
   *   <li>When {@code %}.</li>
   *   <li>Then return {@code %25}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#encodeURI(String)}
   */
  @Test
  public void testEncodeURI_whenPercentSign_thenReturn25() {
    // Arrange, Act and Assert
    assertEquals("%25", Locator.encodeURI("%"));
  }

  /**
   * Test {@link Locator#getToolsJar()}.
   * <p>
   * Method under test: {@link Locator#getToolsJar()}
   */
  @Test
  public void testGetToolsJar() {
    // Arrange, Act and Assert
    assertNull(Locator.getToolsJar());
  }

  /**
   * Test {@link Locator#getLocationURLs(File, String[])} with {@code location}, {@code extensions}.
   * <p>
   * Method under test: {@link Locator#getLocationURLs(File, String[])}
   */
  @Test
  public void testGetLocationURLsWithLocationExtensions() throws MalformedURLException {
    // Arrange, Act and Assert
    assertEquals(0, Locator.getLocationURLs(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "https://example.org/example").length);
  }

  /**
   * Test {@link Locator#getLocationURLs(File, String[])} with {@code location}, {@code extensions}.
   * <p>
   * Method under test: {@link Locator#getLocationURLs(File, String[])}
   */
  @Test
  public void testGetLocationURLsWithLocationExtensions2() throws MalformedURLException {
    // Arrange, Act and Assert
    assertEquals(0, Locator.getLocationURLs(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile(),
        "https://example.org/example").length);
  }

  /**
   * Test {@link Locator#getLocationURLs(File, String[])} with {@code location}, {@code extensions}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getLocationURLs(File, String[])}
   */
  @Test
  public void testGetLocationURLsWithLocationExtensions_whenPropertyIsJavaIoTmpdirIsFooToFile()
      throws MalformedURLException {
    // Arrange, Act and Assert
    assertEquals(0, Locator.getLocationURLs(Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile(),
        "https://example.org/example").length);
  }

  /**
   * Test {@link Locator#getLocationURLs(File)} with {@code location}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getLocationURLs(File)}
   */
  @Test
  public void testGetLocationURLsWithLocation_whenPropertyIsJavaIoTmpdirIsFooToFile() throws MalformedURLException {
    // Arrange, Act and Assert
    assertEquals(0, Locator.getLocationURLs(Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile()).length);
  }

  /**
   * Test {@link Locator#getLocationURLs(File)} with {@code location}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .jar} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getLocationURLs(File)}
   */
  @Test
  public void testGetLocationURLsWithLocation_whenPropertyIsJavaIoTmpdirIsJarToFile() throws MalformedURLException {
    // Arrange, Act and Assert
    assertEquals(0, Locator.getLocationURLs(Paths.get(System.getProperty("java.io.tmpdir"), ".jar").toFile()).length);
  }

  /**
   * Test {@link Locator#getLocationURLs(File)} with {@code location}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Locator#getLocationURLs(File)}
   */
  @Test
  public void testGetLocationURLsWithLocation_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws MalformedURLException {
    // Arrange, Act and Assert
    assertEquals(0,
        Locator.getLocationURLs(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()).length);
  }
}
