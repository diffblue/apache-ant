package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class InnerClassFilenameFilterDiffblueTest {
  /**
   * Test {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)}.
   * <p>
   * Method under test: {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)}
   */
  @Test
  public void testNewInnerClassFilenameFilter() {
    // Arrange and Act
    InnerClassFilenameFilter actualInnerClassFilenameFilter = new InnerClassFilenameFilter("Baseclass");

    // Assert
    assertFalse(actualInnerClassFilenameFilter
        .accept(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "foo.txt"));
  }

  /**
   * Test {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)}.
   * <p>
   * Method under test: {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)}
   */
  @Test
  public void testNewInnerClassFilenameFilter2() {
    // Arrange and Act
    InnerClassFilenameFilter actualInnerClassFilenameFilter = new InnerClassFilenameFilter("Baseclass");
    System.getProperty("java.io.tmpdir");

    // Assert
    assertFalse(actualInnerClassFilenameFilter
        .accept(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), ".class"));
  }

  /**
   * Test {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)}.
   * <p>
   * Method under test: {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)}
   */
  @Test
  public void testNewInnerClassFilenameFilter3() {
    // Arrange and Act
    InnerClassFilenameFilter actualInnerClassFilenameFilter = new InnerClassFilenameFilter(".class");
    System.getProperty("java.io.tmpdir");

    // Assert
    assertTrue(actualInnerClassFilenameFilter
        .accept(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "$"));
  }

  /**
   * Test {@link InnerClassFilenameFilter#accept(File, String)}.
   * <ul>
   *   <li>Given {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)} with baseclass is {@code .class}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InnerClassFilenameFilter#accept(File, String)}
   */
  @Test
  public void testAccept_givenInnerClassFilenameFilterWithBaseclassIsClass_thenReturnTrue() {
    // Arrange
    InnerClassFilenameFilter innerClassFilenameFilter = new InnerClassFilenameFilter(".class");

    // Act and Assert
    assertTrue(
        innerClassFilenameFilter.accept(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "$"));
  }

  /**
   * Test {@link InnerClassFilenameFilter#accept(File, String)}.
   * <ul>
   *   <li>Given {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)} with {@code Baseclass}.</li>
   *   <li>When {@code .class}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InnerClassFilenameFilter#accept(File, String)}
   */
  @Test
  public void testAccept_givenInnerClassFilenameFilterWithBaseclass_whenClass_thenReturnFalse() {
    // Arrange
    InnerClassFilenameFilter innerClassFilenameFilter = new InnerClassFilenameFilter("Baseclass");

    // Act and Assert
    assertFalse(innerClassFilenameFilter.accept(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        ".class"));
  }

  /**
   * Test {@link InnerClassFilenameFilter#accept(File, String)}.
   * <ul>
   *   <li>Given {@link InnerClassFilenameFilter#InnerClassFilenameFilter(String)} with {@code Baseclass}.</li>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InnerClassFilenameFilter#accept(File, String)}
   */
  @Test
  public void testAccept_givenInnerClassFilenameFilterWithBaseclass_whenFooTxt_thenReturnFalse() {
    // Arrange
    InnerClassFilenameFilter innerClassFilenameFilter = new InnerClassFilenameFilter("Baseclass");

    // Act and Assert
    assertFalse(innerClassFilenameFilter.accept(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "foo.txt"));
  }
}
