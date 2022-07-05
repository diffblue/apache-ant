package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Vector;
import org.junit.Test;

public class JavaEnvUtilsDiffblueTest {
  /**
  * Method under test: {@link JavaEnvUtils#getJdkExecutable(String)}
  */
  @Test
  public void testGetJdkExecutable() {
    // Arrange, Act and Assert
    assertEquals("Command", JavaEnvUtils.getJdkExecutable("Command"));
  }

  /**
   * Method under test: {@link JavaEnvUtils#getJdkExecutable(String)}
   */
  @Test
  public void testGetJdkExecutable2() {
    // Arrange and Act
    String actualJdkExecutable = JavaEnvUtils.getJdkExecutable("/../bin");

    // Assert
    assertEquals(Paths
        .get(System.getProperty("user.home"), ".sdkman", "candidates", "java", "8.0.322-librca", "bin", "..", "bin")
        .toString(), actualJdkExecutable);
  }

  /**
   * Method under test: {@link JavaEnvUtils#getJdkExecutable(String)}
   */
  @Test
  public void testGetJdkExecutable3() {
    // Arrange and Act
    String actualJdkExecutable = JavaEnvUtils.getJdkExecutable("../../bin");

    // Assert
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", "..", "..", "bin").toString(), actualJdkExecutable);
  }

  /**
   * Method under test: {@link JavaEnvUtils#getJreExecutable(String)}
   */
  @Test
  public void testGetJreExecutable() {
    // Arrange, Act and Assert
    assertEquals("Command", JavaEnvUtils.getJreExecutable("Command"));
  }

  /**
   * Method under test: {@link JavaEnvUtils#getJreExecutable(String)}
   */
  @Test
  public void testGetJreExecutable2() {
    // Arrange and Act
    String actualJreExecutable = JavaEnvUtils.getJreExecutable(".");

    // Assert
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", ".").toString(), actualJreExecutable);
  }

  /**
   * Method under test: {@link JavaEnvUtils#getJrePackageTestCases()}
   */
  @Test
  public void testGetJrePackageTestCases() {
    // Arrange and Act
    Vector<String> actualJrePackageTestCases = JavaEnvUtils.getJrePackageTestCases();

    // Assert
    assertEquals(21, actualJrePackageTestCases.size());
    assertEquals("java.lang.Object", actualJrePackageTestCases.get(0));
    assertEquals("sun.reflect.SerializationConstructorAccessorImpl", actualJrePackageTestCases.get(1));
    assertEquals("sun.net.www.http.HttpClient", actualJrePackageTestCases.get(2));
    assertEquals("sun.audio.AudioPlayer", actualJrePackageTestCases.get(3));
    assertEquals("javax.accessibility.Accessible", actualJrePackageTestCases.get(4));
    assertEquals("sun.misc.BASE64Encoder", actualJrePackageTestCases.get(5));
    assertEquals("sun.audio.AudioPlayer", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_5));
    assertEquals("org.ietf.jgss.Oid", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_6));
    assertEquals("org.w3c.dom.Attr", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_7));
    assertEquals("org.xml.sax.XMLReader", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_8));
    assertEquals("com.sun.org.apache.xerces.internal.jaxp.datatype.DatatypeFactoryImpl",
        actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_9));
    assertEquals("jdk.net.Sockets", actualJrePackageTestCases.get(20));
  }

  /**
   * Method under test: {@link JavaEnvUtils#isAtLeastJavaVersion(String)}
   */
  @Test
  public void testIsAtLeastJavaVersion() {
    // Arrange, Act and Assert
    assertTrue(JavaEnvUtils.isAtLeastJavaVersion("1.0.2"));
    assertFalse(JavaEnvUtils.isAtLeastJavaVersion("42"));
    assertTrue(JavaEnvUtils.isAtLeastJavaVersion(""));
  }

  /**
   * Method under test: {@link JavaEnvUtils#isJavaVersion(String)}
   */
  @Test
  public void testIsJavaVersion() {
    // Arrange, Act and Assert
    assertFalse(JavaEnvUtils.isJavaVersion("1.0.2"));
    assertTrue(JavaEnvUtils.isJavaVersion(JavaEnvUtils.JAVA_1_8));
  }
}

