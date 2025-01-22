package org.apache.tools.ant.util;

import static org.junit.Assert.assertFalse;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.junit.Test;

public class SymbolicLinkUtilsDiffblueTest {
  /**
   * Test {@link SymbolicLinkUtils#isSymbolicLink(File)} with {@code file}.
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(File)}
   */
  @Test
  public void testIsSymbolicLinkWithFile() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SymbolicLinkUtils#isSymbolicLink(String)} with {@code name}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(String)}
   */
  @Test
  public void testIsSymbolicLinkWithName_whenName_thenReturnFalse() throws IOException {
    // Arrange, Act and Assert
    assertFalse(SymbolicLinkUtils.getSymbolicLinkUtils().isSymbolicLink("Name"));
  }

  /**
   * Test {@link SymbolicLinkUtils#isSymbolicLink(String)} with {@code name}.
   * <ul>
   *   <li>When Property is {@code user.dir} is {@code Name} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(String)}
   */
  @Test
  public void testIsSymbolicLinkWithName_whenPropertyIsUserDirIsNameToString_thenReturnFalse() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils.isSymbolicLink(Paths.get(System.getProperty("user.dir"), "Name").toString()));
  }

  /**
   * Test {@link SymbolicLinkUtils#isSymbolicLink(File, String)} with {@code parent}, {@code name}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLinkWithParentName_whenName_thenReturnFalse() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(
        symbolicLinkUtils.isSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)} with {@code file}.
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithFile() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "42", "", "foo").toFile()));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)} with {@code file}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithFile_whenPropertyIsJavaIoTmpdirIs42And42ToFile() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)} with {@code file}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithFile_whenPropertyIsJavaIoTmpdirIs42AndFooToFile() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "42", "foo", "foo").toFile()));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)} with {@code file}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(
        symbolicLinkUtils.isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)} with {@code parent}, {@code name}.
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithParentName() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)} with {@code parent}, {@code name}.
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithParentName2() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "foo", "foo", "foo").toFile(), "Name"));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)} with {@code parent}, {@code name}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithParentName_whenEmptyString_thenReturnFalse() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), ""));
  }

  /**
   * Test {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)} with {@code parent}, {@code name}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code foo} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymbolicLinkUtils#isDanglingSymbolicLink(File, String)}
   */
  @Test
  public void testIsDanglingSymbolicLinkWithParentName_whenPropertyIsJavaIoTmpdirIsFooToFile() throws IOException {
    // Arrange
    SymbolicLinkUtils symbolicLinkUtils = SymbolicLinkUtils.getSymbolicLinkUtils();

    // Act and Assert
    assertFalse(symbolicLinkUtils
        .isDanglingSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile(), "Name"));
  }
}
