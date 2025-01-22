package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class ExtensionAdapterDiffblueTest {
  /**
   * Test {@link ExtensionAdapter#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link ExtensionAdapter} (default constructor).</li>
   *   <li>Then {@link ExtensionAdapter} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionAdapter#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenExtensionAdapter_thenExtensionAdapterReference() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    Reference reference = new Reference("42");

    // Act
    extensionAdapter.setRefid(reference);

    // Assert
    assertTrue(extensionAdapter.isReference());
    assertSame(reference, extensionAdapter.getRefid());
  }

  /**
   * Test {@link ExtensionAdapter#toExtension()}.
   * <ul>
   *   <li>Given {@link ExtensionAdapter} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionAdapter#toExtension()}
   */
  @Test
  public void testToExtension_givenExtensionAdapter_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ExtensionAdapter()).toExtension());
  }

  /**
   * Test {@link ExtensionAdapter#toExtension()}.
   * <ul>
   *   <li>Then return ImplementationVersion is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionAdapter#toExtension()}
   */
  @Test
  public void testToExtension_thenReturnImplementationVersionIsNull() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setExtensionName("Extension is missing name.");

    // Act
    Extension actualToExtensionResult = extensionAdapter.toExtension();

    // Assert
    assertEquals("Extension is missing name.", actualToExtensionResult.getExtensionName());
    assertNull(actualToExtensionResult.getImplementationURL());
    assertNull(actualToExtensionResult.getImplementationVendor());
    assertNull(actualToExtensionResult.getImplementationVendorID());
    assertNull(actualToExtensionResult.getSpecificationVendor());
    assertNull(actualToExtensionResult.getImplementationVersion());
    assertNull(actualToExtensionResult.getSpecificationVersion());
  }

  /**
   * Test {@link ExtensionAdapter#toExtension()}.
   * <ul>
   *   <li>Then return ImplementationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionAdapter#toExtension()}
   */
  @Test
  public void testToExtension_thenReturnImplementationVersionSizeIsThree() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setImplementationVersion("1.0.2");
    extensionAdapter.setExtensionName("Extension is missing name.");

    // Act
    Extension actualToExtensionResult = extensionAdapter.toExtension();

    // Assert
    assertEquals("Extension is missing name.", actualToExtensionResult.getExtensionName());
    assertNull(actualToExtensionResult.getImplementationURL());
    assertNull(actualToExtensionResult.getImplementationVendor());
    assertNull(actualToExtensionResult.getImplementationVendorID());
    assertNull(actualToExtensionResult.getSpecificationVendor());
    assertNull(actualToExtensionResult.getSpecificationVersion());
    assertEquals(3, actualToExtensionResult.getImplementationVersion().getSize());
  }

  /**
   * Test {@link ExtensionAdapter#toExtension()}.
   * <ul>
   *   <li>Then return SpecificationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionAdapter#toExtension()}
   */
  @Test
  public void testToExtension_thenReturnSpecificationVersionSizeIsThree() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setSpecificationVersion("1.0.2");
    extensionAdapter.setExtensionName("Extension is missing name.");

    // Act
    Extension actualToExtensionResult = extensionAdapter.toExtension();

    // Assert
    assertEquals("Extension is missing name.", actualToExtensionResult.getExtensionName());
    assertNull(actualToExtensionResult.getImplementationURL());
    assertNull(actualToExtensionResult.getImplementationVendor());
    assertNull(actualToExtensionResult.getImplementationVendorID());
    assertNull(actualToExtensionResult.getSpecificationVendor());
    assertNull(actualToExtensionResult.getImplementationVersion());
    assertEquals(3, actualToExtensionResult.getSpecificationVersion().getSize());
  }

  /**
   * Test new {@link ExtensionAdapter} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ExtensionAdapter}
   */
  @Test
  public void testNewExtensionAdapter() {
    // Arrange and Act
    ExtensionAdapter actualExtensionAdapter = new ExtensionAdapter();

    // Assert
    Location location = actualExtensionAdapter.getLocation();
    assertNull(location.getFileName());
    assertNull(actualExtensionAdapter.getDescription());
    assertNull(actualExtensionAdapter.getProject());
    assertNull(actualExtensionAdapter.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualExtensionAdapter.isReference());
  }
}
