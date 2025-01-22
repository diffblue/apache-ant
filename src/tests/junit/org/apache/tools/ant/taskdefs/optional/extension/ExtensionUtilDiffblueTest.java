package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.util.DeweyDecimal;
import org.junit.Test;

public class ExtensionUtilDiffblueTest {
  /**
   * Test {@link ExtensionUtil#toExtensions(List)}.
   * <ul>
   *   <li>Then return first ImplementationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionUtil#toExtensions(List)}
   */
  @Test
  public void testToExtensions_thenReturnFirstImplementationVersionSizeIsThree() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setExtensionName("Extension is missing name.");

    ExtensionAdapter extensionAdapter2 = new ExtensionAdapter();
    extensionAdapter2.setImplementationVersion("1.0.2");
    extensionAdapter2.setExtensionName("Extension is missing name.");

    ArrayList<ExtensionAdapter> adapters = new ArrayList<>();
    adapters.add(extensionAdapter2);
    adapters.add(extensionAdapter);

    // Act
    ArrayList<Extension> actualToExtensionsResult = ExtensionUtil.toExtensions(adapters);

    // Assert
    assertEquals(2, actualToExtensionsResult.size());
    Extension getResult = actualToExtensionsResult.get(1);
    assertEquals("Extension is missing name.", getResult.getExtensionName());
    assertNull(getResult.getImplementationURL());
    assertNull(getResult.getImplementationVendor());
    assertNull(getResult.getImplementationVendorID());
    assertNull(getResult.getSpecificationVendor());
    assertNull(getResult.getImplementationVersion());
    Extension getResult2 = actualToExtensionsResult.get(0);
    assertNull(getResult2.getSpecificationVersion());
    assertNull(getResult.getSpecificationVersion());
    assertEquals(3, getResult2.getImplementationVersion().getSize());
  }

  /**
   * Test {@link ExtensionUtil#toExtensions(List)}.
   * <ul>
   *   <li>Then return first SpecificationVersion is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionUtil#toExtensions(List)}
   */
  @Test
  public void testToExtensions_thenReturnFirstSpecificationVersionIsNull() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setExtensionName("Extension is missing name.");

    ExtensionAdapter extensionAdapter2 = new ExtensionAdapter();
    extensionAdapter2.setExtensionName("Extension is missing name.");

    ArrayList<ExtensionAdapter> adapters = new ArrayList<>();
    adapters.add(extensionAdapter2);
    adapters.add(extensionAdapter);

    // Act
    ArrayList<Extension> actualToExtensionsResult = ExtensionUtil.toExtensions(adapters);

    // Assert
    assertEquals(2, actualToExtensionsResult.size());
    Extension getResult = actualToExtensionsResult.get(1);
    assertEquals("Extension is missing name.", getResult.getExtensionName());
    assertNull(getResult.getImplementationURL());
    assertNull(getResult.getImplementationVendor());
    assertNull(getResult.getImplementationVendorID());
    assertNull(getResult.getSpecificationVendor());
    Extension getResult2 = actualToExtensionsResult.get(0);
    assertNull(getResult2.getImplementationVersion());
    assertNull(getResult.getImplementationVersion());
    assertNull(getResult2.getSpecificationVersion());
    assertNull(getResult.getSpecificationVersion());
  }

  /**
   * Test {@link ExtensionUtil#toExtensions(List)}.
   * <ul>
   *   <li>Then return first SpecificationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionUtil#toExtensions(List)}
   */
  @Test
  public void testToExtensions_thenReturnFirstSpecificationVersionSizeIsThree() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setExtensionName("Extension is missing name.");

    ExtensionAdapter extensionAdapter2 = new ExtensionAdapter();
    extensionAdapter2.setSpecificationVersion("1.0.2");
    extensionAdapter2.setExtensionName("Extension is missing name.");

    ArrayList<ExtensionAdapter> adapters = new ArrayList<>();
    adapters.add(extensionAdapter2);
    adapters.add(extensionAdapter);

    // Act
    ArrayList<Extension> actualToExtensionsResult = ExtensionUtil.toExtensions(adapters);

    // Assert
    assertEquals(2, actualToExtensionsResult.size());
    Extension getResult = actualToExtensionsResult.get(1);
    assertEquals("Extension is missing name.", getResult.getExtensionName());
    assertNull(getResult.getImplementationURL());
    assertNull(getResult.getImplementationVendor());
    assertNull(getResult.getImplementationVendorID());
    assertNull(getResult.getSpecificationVendor());
    Extension getResult2 = actualToExtensionsResult.get(0);
    assertNull(getResult2.getImplementationVersion());
    assertNull(getResult.getImplementationVersion());
    assertNull(getResult.getSpecificationVersion());
    assertEquals(3, getResult2.getSpecificationVersion().getSize());
  }

  /**
   * Test {@link ExtensionUtil#toExtensions(List)}.
   * <ul>
   *   <li>Then return second SpecificationVersion is first SpecificationVersion.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionUtil#toExtensions(List)}
   */
  @Test
  public void testToExtensions_thenReturnSecondSpecificationVersionIsFirstSpecificationVersion() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setSpecificationVersion("1.0.2");
    extensionAdapter.setExtensionName("Extension is missing name.");

    ExtensionAdapter extensionAdapter2 = new ExtensionAdapter();
    extensionAdapter2.setSpecificationVersion("1.0.2");
    extensionAdapter2.setExtensionName("Extension is missing name.");

    ArrayList<ExtensionAdapter> adapters = new ArrayList<>();
    adapters.add(extensionAdapter2);
    adapters.add(extensionAdapter);

    // Act
    ArrayList<Extension> actualToExtensionsResult = ExtensionUtil.toExtensions(adapters);

    // Assert
    assertEquals(2, actualToExtensionsResult.size());
    Extension getResult = actualToExtensionsResult.get(1);
    assertEquals("Extension is missing name.", getResult.getExtensionName());
    assertNull(getResult.getImplementationURL());
    assertNull(getResult.getImplementationVendor());
    assertNull(getResult.getImplementationVendorID());
    assertNull(getResult.getSpecificationVendor());
    Extension getResult2 = actualToExtensionsResult.get(0);
    assertNull(getResult2.getImplementationVersion());
    assertNull(getResult.getImplementationVersion());
    DeweyDecimal specificationVersion = getResult2.getSpecificationVersion();
    assertEquals(3, specificationVersion.getSize());
    assertEquals(specificationVersion, getResult.getSpecificationVersion());
  }

  /**
   * Test {@link ExtensionUtil#toExtensions(List)}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionUtil#toExtensions(List)}
   */
  @Test
  public void testToExtensions_thenReturnSizeIsOne() throws BuildException {
    // Arrange
    ExtensionAdapter extensionAdapter = new ExtensionAdapter();
    extensionAdapter.setExtensionName("Extension is missing name.");

    ArrayList<ExtensionAdapter> adapters = new ArrayList<>();
    adapters.add(extensionAdapter);

    // Act
    ArrayList<Extension> actualToExtensionsResult = ExtensionUtil.toExtensions(adapters);

    // Assert
    assertEquals(1, actualToExtensionsResult.size());
    Extension getResult = actualToExtensionsResult.get(0);
    assertEquals("Extension is missing name.", getResult.getExtensionName());
    assertNull(getResult.getImplementationURL());
    assertNull(getResult.getImplementationVendor());
    assertNull(getResult.getImplementationVendorID());
    assertNull(getResult.getSpecificationVendor());
  }

  /**
   * Test {@link ExtensionUtil#toExtensions(List)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExtensionUtil#toExtensions(List)}
   */
  @Test
  public void testToExtensions_whenArrayList_thenReturnEmpty() throws BuildException {
    // Arrange and Act
    ArrayList<Extension> actualToExtensionsResult = ExtensionUtil.toExtensions(new ArrayList<>());

    // Assert
    assertTrue(actualToExtensionsResult.isEmpty());
  }

  /**
   * Test {@link ExtensionUtil#getManifest(File)}.
   * <p>
   * Method under test: {@link ExtensionUtil#getManifest(File)}
   */
  @Test
  public void testGetManifest() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> ExtensionUtil.getManifest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
