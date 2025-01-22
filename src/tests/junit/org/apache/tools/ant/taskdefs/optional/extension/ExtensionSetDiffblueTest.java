package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class ExtensionSetDiffblueTest {
  /**
   * Test {@link ExtensionSet#toExtensions(Project)}.
   * <p>
   * Method under test: {@link ExtensionSet#toExtensions(Project)}
   */
  @Test
  public void testToExtensions() throws BuildException {
    // Arrange
    LibFileSet fileSet = new LibFileSet();
    fileSet.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    ExtensionSet extensionSet = new ExtensionSet();
    extensionSet.addLibfileset(fileSet);

    // Act and Assert
    assertEquals(0, extensionSet.toExtensions(new Project()).length);
  }

  /**
   * Test {@link ExtensionSet#toExtensions(Project)}.
   * <ul>
   *   <li>Given {@link ExtensionSet} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionSet#toExtensions(Project)}
   */
  @Test
  public void testToExtensions_givenExtensionSet_thenReturnArrayLengthIsZero() throws BuildException {
    // Arrange
    ExtensionSet extensionSet = new ExtensionSet();

    // Act and Assert
    assertEquals(0, extensionSet.toExtensions(new Project()).length);
  }

  /**
   * Test {@link ExtensionSet#toExtensions(Project)}.
   * <ul>
   *   <li>Then return first element ExtensionName is {@code Extension is missing name.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionSet#toExtensions(Project)}
   */
  @Test
  public void testToExtensions_thenReturnFirstElementExtensionNameIsExtensionIsMissingName() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setExtensionName("Extension is missing name.");

    ExtensionSet extensionSet = new ExtensionSet();
    extensionSet.addExtension(extensionAdapter);

    // Act
    Extension[] actualToExtensionsResult = extensionSet.toExtensions(new Project());

    // Assert
    Extension extension = actualToExtensionsResult[0];
    assertEquals("Extension is missing name.", extension.getExtensionName());
    assertNull(extension.getImplementationURL());
    assertNull(extension.getImplementationVendor());
    assertNull(extension.getImplementationVendorID());
    assertNull(extension.getSpecificationVendor());
    assertNull(extension.getImplementationVersion());
    assertNull(extension.getSpecificationVersion());
    assertEquals(1, actualToExtensionsResult.length);
  }

  /**
   * Test {@link ExtensionSet#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link ExtensionSet} (default constructor).</li>
   *   <li>Then {@link ExtensionSet} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionSet#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenExtensionSet_thenExtensionSetReference() throws BuildException {
    // Arrange
    ExtensionSet extensionSet = new ExtensionSet();
    Reference reference = new Reference("42");

    // Act
    extensionSet.setRefid(reference);

    // Assert
    assertTrue(extensionSet.isReference());
    assertSame(reference, extensionSet.getRefid());
  }

  /**
   * Test new {@link ExtensionSet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ExtensionSet}
   */
  @Test
  public void testNewExtensionSet() {
    // Arrange and Act
    ExtensionSet actualExtensionSet = new ExtensionSet();

    // Assert
    Location location = actualExtensionSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExtensionSet.getDescription());
    assertNull(actualExtensionSet.getProject());
    assertNull(actualExtensionSet.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualExtensionSet.isReference());
  }

  /**
   * Test {@link ExtensionSet#toString()}.
   * <p>
   * Method under test: {@link ExtensionSet#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("ExtensionSet[]", (new ExtensionSet()).toString());
  }
}
