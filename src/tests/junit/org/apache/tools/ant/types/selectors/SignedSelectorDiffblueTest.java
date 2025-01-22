package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class SignedSelectorDiffblueTest {
  /**
   * Test {@link SignedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SignedSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSignedSelector() {
    // Arrange
    SignedSelector signedSelector = new SignedSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link SignedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SignedSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSignedSelector2() {
    // Arrange
    SignedSelector signedSelector = new SignedSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test {@link SignedSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Given {@link SignedSelector} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SignedSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_givenSignedSelectorProjectIsProject() {
    // Arrange
    SignedSelector signedSelector = new SignedSelector();
    signedSelector.setProject(new Project());
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(signedSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test new {@link SignedSelector} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SignedSelector}
   */
  @Test
  public void testNewSignedSelector() {
    // Arrange and Act
    SignedSelector actualSignedSelector = new SignedSelector();

    // Assert
    Location location = actualSignedSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSignedSelector.getDescription());
    assertNull(actualSignedSelector.getProject());
    assertNull(actualSignedSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualSignedSelector.isReference());
  }
}
