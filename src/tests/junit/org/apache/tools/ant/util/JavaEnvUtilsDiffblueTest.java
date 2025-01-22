package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Vector;
import org.junit.Test;

public class JavaEnvUtilsDiffblueTest {
  /**
   * Test {@link JavaEnvUtils#isJavaVersion(String)}.
   * <ul>
   *   <li>When {@code 17}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#isJavaVersion(String)}
   */
  @Test
  public void testIsJavaVersion_when17_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(JavaEnvUtils.isJavaVersion("17"));
  }

  /**
   * Test {@link JavaEnvUtils#isJavaVersion(String)}.
   * <ul>
   *   <li>When {@code 1.0.2}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#isJavaVersion(String)}
   */
  @Test
  public void testIsJavaVersion_when102_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(JavaEnvUtils.isJavaVersion("1.0.2"));
  }

  /**
   * Test {@link JavaEnvUtils#isAtLeastJavaVersion(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#isAtLeastJavaVersion(String)}
   */
  @Test
  public void testIsAtLeastJavaVersion_when42_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(JavaEnvUtils.isAtLeastJavaVersion("42"));
  }

  /**
   * Test {@link JavaEnvUtils#isAtLeastJavaVersion(String)}.
   * <ul>
   *   <li>When {@code 1.0.2}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#isAtLeastJavaVersion(String)}
   */
  @Test
  public void testIsAtLeastJavaVersion_when102_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(JavaEnvUtils.isAtLeastJavaVersion("1.0.2"));
  }

  /**
   * Test {@link JavaEnvUtils#isAtLeastJavaVersion(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#isAtLeastJavaVersion(String)}
   */
  @Test
  public void testIsAtLeastJavaVersion_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(JavaEnvUtils.isAtLeastJavaVersion(""));
  }

  /**
   * Test {@link JavaEnvUtils#getJreExecutable(String)}.
   * <p>
   * Method under test: {@link JavaEnvUtils#getJreExecutable(String)}
   */
  @Test
  public void testGetJreExecutable() {
    // Arrange and Act
    String actualJreExecutable = JavaEnvUtils.getJreExecutable(".");

    // Assert
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", ".").toString(), actualJreExecutable);
  }

  /**
   * Test {@link JavaEnvUtils#getJreExecutable(String)}.
   * <ul>
   *   <li>When {@code Command}.</li>
   *   <li>Then return {@code Command}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#getJreExecutable(String)}
   */
  @Test
  public void testGetJreExecutable_whenCommand_thenReturnCommand() {
    // Arrange, Act and Assert
    assertEquals("Command", JavaEnvUtils.getJreExecutable("Command"));
  }

  /**
   * Test {@link JavaEnvUtils#getJdkExecutable(String)}.
   * <p>
   * Method under test: {@link JavaEnvUtils#getJdkExecutable(String)}
   */
  @Test
  public void testGetJdkExecutable() {
    // Arrange and Act
    String actualJdkExecutable = JavaEnvUtils.getJdkExecutable("/../bin");

    // Assert
    assertEquals(Paths.get(System.getProperty("java.home"), "bin", "..", "bin").toString(), actualJdkExecutable);
  }

  /**
   * Test {@link JavaEnvUtils#getJdkExecutable(String)}.
   * <ul>
   *   <li>When {@code Command}.</li>
   *   <li>Then return {@code Command}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaEnvUtils#getJdkExecutable(String)}
   */
  @Test
  public void testGetJdkExecutable_whenCommand_thenReturnCommand() {
    // Arrange, Act and Assert
    assertEquals("Command", JavaEnvUtils.getJdkExecutable("Command"));
  }

  /**
   * Test {@link JavaEnvUtils#getJrePackageTestCases()}.
   * <p>
   * Method under test: {@link JavaEnvUtils#getJrePackageTestCases()}
   */
  @Test
  public void testGetJrePackageTestCases() {
    // Arrange and Act
    Vector<String> actualJrePackageTestCases = JavaEnvUtils.getJrePackageTestCases();

    // Assert
    assertEquals(21, actualJrePackageTestCases.size());
    assertEquals("com.sun.org.apache.xerces.internal.jaxp.datatype.DatatypeFactoryImpl",
        actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_9));
    assertEquals("java.lang.Object", actualJrePackageTestCases.get(0));
    assertEquals("javax.accessibility.Accessible", actualJrePackageTestCases.get(4));
    assertEquals("jdk.net.Sockets", actualJrePackageTestCases.get(20));
    assertEquals("org.ietf.jgss.Oid", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_6));
    assertEquals("org.w3c.dom.Attr", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_7));
    assertEquals("org.xml.sax.XMLReader", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_8));
    assertEquals("sun.audio.AudioPlayer", actualJrePackageTestCases.get(3));
    assertEquals("sun.audio.AudioPlayer", actualJrePackageTestCases.get(JavaEnvUtils.VERSION_1_5));
    assertEquals("sun.misc.BASE64Encoder", actualJrePackageTestCases.get(5));
    assertEquals("sun.net.www.http.HttpClient", actualJrePackageTestCases.get(2));
    assertEquals("sun.reflect.SerializationConstructorAccessorImpl", actualJrePackageTestCases.get(1));
  }
}
