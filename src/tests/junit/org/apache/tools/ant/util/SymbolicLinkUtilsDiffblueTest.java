package org.apache.tools.ant.util;

import static org.junit.Assert.assertFalse;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.junit.Test;

public class SymbolicLinkUtilsDiffblueTest {
  /**
  * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
  */
  @Test
  public void testIsDanglingSymbolicLink() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(
        symbolicLinkUtils.isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
   */
  @Test
  public void testIsDanglingSymbolicLink2() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(
        symbolicLinkUtils.isDanglingSymbolicLink(Paths.get(System.getProperty("test.txt"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
   */
  @Test
  public void testIsDanglingSymbolicLink3() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isDanglingSymbolicLink(
        Paths
            .get(System.getProperty("java.io.tmpdir"),
                "SavedLayoutPreservingPropertiesDiffblueTest8573777134726363230.java")
            .toFile()));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLink4() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLink5() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(
        symbolicLinkUtils.isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile(), "Name"));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLink6() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "").toFile(),
        "SavedLayoutPreservingPropertiesDiffblueTest8573777134726363230.java"));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(File)}
   */
  @Test
  public void testIsSymbolicLink() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(File)}
   */
  @Test
  public void testIsSymbolicLink2() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isSymbolicLink(Paths.get("", "test.txt").toFile()));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLink3() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(
        symbolicLinkUtils.isSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLink4() throws IOException {
    // Arrange, Act and Assert
    assertFalse(SymbolicLinkUtils.getSymbolicLinkUtils().isSymbolicLink(null, "foo"));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(String)}
   */
  @Test
  public void testIsSymbolicLink5() throws IOException {
    // Arrange, Act and Assert
    assertFalse(SymbolicLinkUtils.getSymbolicLinkUtils().isSymbolicLink("Name"));
  }

  /**
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(String)}
   */
  @Test
  public void testIsSymbolicLink6() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isSymbolicLink(Paths.get(System.getProperty("user.dir"), "Name").toString()));
  }
}

