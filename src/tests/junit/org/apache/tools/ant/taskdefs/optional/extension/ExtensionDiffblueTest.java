package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import org.apache.tools.ant.util.DeweyDecimal;
import org.junit.Test;

public class ExtensionDiffblueTest {
  /**
   * Test {@link Extension#getAvailable(Manifest)}.
   * <ul>
   *   <li>When {@link Manifest#Manifest()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getAvailable(Manifest)}
   */
  @Test
  public void testGetAvailable_whenManifest() {
    // Arrange, Act and Assert
    assertEquals(0, Extension.getAvailable(new Manifest()).length);
  }

  /**
   * Test {@link Extension#getAvailable(Manifest)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getAvailable(Manifest)}
   */
  @Test
  public void testGetAvailable_whenNull() {
    // Arrange, Act and Assert
    assertEquals(0, Extension.getAvailable(null).length);
  }

  /**
   * Test {@link Extension#getRequired(Manifest)}.
   * <ul>
   *   <li>When {@link Manifest#Manifest()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getRequired(Manifest)}
   */
  @Test
  public void testGetRequired_whenManifest_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Extension.getRequired(new Manifest()).length);
  }

  /**
   * Test {@link Extension#getOptions(Manifest)}.
   * <ul>
   *   <li>When {@link Manifest#Manifest()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getOptions(Manifest)}
   */
  @Test
  public void testGetOptions_whenManifest_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Extension.getOptions(new Manifest()).length);
  }

  /**
   * Test {@link Extension#addExtension(Extension, Attributes)} with {@code extension}, {@code attributes}.
   * <ul>
   *   <li>Then {@link Attributes#Attributes()} size is seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#addExtension(Extension, Attributes)}
   */
  @Test
  public void testAddExtensionWithExtensionAttributes_thenAttributesSizeIsSeven() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    Attributes attributes = new Attributes();

    // Act
    Extension.addExtension(extension, attributes);

    // Assert
    assertEquals(7, attributes.size());
  }

  /**
   * Test {@link Extension#addExtension(Extension, String, Attributes)} with {@code extension}, {@code prefix}, {@code attributes}.
   * <ul>
   *   <li>Then {@link Attributes#Attributes()} size is seven.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#addExtension(Extension, String, Attributes)}
   */
  @Test
  public void testAddExtensionWithExtensionPrefixAttributes_thenAttributesSizeIsSeven() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    Attributes attributes = new Attributes();

    // Act
    Extension.addExtension(extension, "Prefix", attributes);

    // Assert
    assertEquals(7, attributes.size());
  }

  /**
   * Test {@link Extension#Extension(String, String, String, String, String, String, String)}.
   * <ul>
   *   <li>When {@code 1.0.2}.</li>
   *   <li>Then return ImplementationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#Extension(String, String, String, String, String, String, String)}
   */
  @Test
  public void testNewExtension_when102_thenReturnImplementationVersionSizeIsThree() {
    // Arrange and Act
    Extension actualExtension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Assert
    assertEquals("42", actualExtension.getImplementationVendorID());
    assertEquals("Extension Name", actualExtension.getExtensionName());
    assertEquals("Implementation Vendor", actualExtension.getImplementationVendor());
    assertEquals("Specification Vendor", actualExtension.getSpecificationVendor());
    assertEquals("https://example.org/example", actualExtension.getImplementationURL());
    DeweyDecimal implementationVersion = actualExtension.getImplementationVersion();
    assertEquals(3, implementationVersion.getSize());
    assertEquals(implementationVersion, actualExtension.getSpecificationVersion());
  }

  /**
   * Test {@link Extension#Extension(String, String, String, String, String, String, String)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#Extension(String, String, String, String, String, String, String)}
   */
  @Test
  public void testNewExtension_whenDot_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new Extension("Extension Name", ".", "Specification Vendor",
        "1.0.2", "Implementation Vendor", "42", "https://example.org/example"));

  }

  /**
   * Test {@link Extension#Extension(String, String, String, String, String, String, String)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#Extension(String, String, String, String, String, String, String)}
   */
  @Test
  public void testNewExtension_whenDot_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new Extension("Extension Name", "1.0.2", "Specification Vendor",
        ".", "Implementation Vendor", "42", "https://example.org/example"));

  }

  /**
   * Test {@link Extension#Extension(String, String, String, String, String, String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return ImplementationVersion is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#Extension(String, String, String, String, String, String, String)}
   */
  @Test
  public void testNewExtension_whenNull_thenReturnImplementationVersionIsNull() {
    // Arrange and Act
    Extension actualExtension = new Extension("Extension Name", null, "Specification Vendor", null,
        "Implementation Vendor", "42", "https://example.org/example");

    // Assert
    assertEquals("42", actualExtension.getImplementationVendorID());
    assertEquals("Extension Name", actualExtension.getExtensionName());
    assertEquals("Implementation Vendor", actualExtension.getImplementationVendor());
    assertEquals("Specification Vendor", actualExtension.getSpecificationVendor());
    assertEquals("https://example.org/example", actualExtension.getImplementationURL());
    assertNull(actualExtension.getImplementationVersion());
    assertNull(actualExtension.getSpecificationVersion());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Extension#getExtensionName()}
   *   <li>{@link Extension#getImplementationURL()}
   *   <li>{@link Extension#getImplementationVendor()}
   *   <li>{@link Extension#getImplementationVendorID()}
   *   <li>{@link Extension#getImplementationVersion()}
   *   <li>{@link Extension#getSpecificationVendor()}
   *   <li>{@link Extension#getSpecificationVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    String actualExtensionName = extension.getExtensionName();
    String actualImplementationURL = extension.getImplementationURL();
    String actualImplementationVendor = extension.getImplementationVendor();
    String actualImplementationVendorID = extension.getImplementationVendorID();
    DeweyDecimal actualImplementationVersion = extension.getImplementationVersion();
    String actualSpecificationVendor = extension.getSpecificationVendor();
    DeweyDecimal actualSpecificationVersion = extension.getSpecificationVersion();

    // Assert
    assertEquals("42", actualImplementationVendorID);
    assertEquals("Extension Name", actualExtensionName);
    assertEquals("Implementation Vendor", actualImplementationVendor);
    assertEquals("Specification Vendor", actualSpecificationVendor);
    assertEquals("https://example.org/example", actualImplementationURL);
    assertEquals(3, actualImplementationVersion.getSize());
    assertEquals(3, actualSpecificationVersion.getSize());
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");
    Extension required = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("COMPATIBLE", actualCompatibilityWith.toString());
    assertSame(required.COMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith2() {
    // Arrange
    Extension extension = new Extension("Extension Name", "42", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");
    Extension required = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("COMPATIBLE", actualCompatibilityWith.toString());
    assertSame(required.COMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith3() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");
    Extension required = new Extension("Extension Name", "", "Specification Vendor", "1.0.2", "Implementation Vendor",
        "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("COMPATIBLE", actualCompatibilityWith.toString());
    assertSame(required.COMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <ul>
   *   <li>Then return toString is {@code INCOMPATIBLE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsIncompatible() {
    // Arrange
    Extension extension = new Extension("42", "1.0.2", "Specification Vendor", "1.0.2", "Implementation Vendor", "42",
        "https://example.org/example");
    Extension required = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("INCOMPATIBLE", actualCompatibilityWith.toString());
    assertSame(required.INCOMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <ul>
   *   <li>Then return toString is {@code REQUIRE_IMPLEMENTATION_UPGRADE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsRequireImplementationUpgrade() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "", "Implementation Vendor",
        "42", "https://example.org/example");
    Extension required = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("REQUIRE_IMPLEMENTATION_UPGRADE", actualCompatibilityWith.toString());
    assertSame(required.REQUIRE_IMPLEMENTATION_UPGRADE, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <ul>
   *   <li>Then return toString is {@code REQUIRE_SPECIFICATION_UPGRADE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsRequireSpecificationUpgrade() {
    // Arrange
    Extension extension = new Extension("Extension Name", "", "Specification Vendor", "1.0.2", "Implementation Vendor",
        "42", "https://example.org/example");
    Extension required = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("REQUIRE_SPECIFICATION_UPGRADE", actualCompatibilityWith.toString());
    assertSame(required.REQUIRE_SPECIFICATION_UPGRADE, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#getCompatibilityWith(Extension)}.
   * <ul>
   *   <li>Then return toString is {@code REQUIRE_VENDOR_SWITCH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#getCompatibilityWith(Extension)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsRequireVendorSwitch() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "Extension Name", "https://example.org/example");
    Extension required = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act
    Compatibility actualCompatibilityWith = extension.getCompatibilityWith(required);

    // Assert
    assertEquals("REQUIRE_VENDOR_SWITCH", actualCompatibilityWith.toString());
    assertSame(required.REQUIRE_VENDOR_SWITCH, actualCompatibilityWith);
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith() {
    // Arrange
    Extension extension = new Extension("42", "1.0.2", "Specification Vendor", "1.0.2", "Implementation Vendor", "42",
        "https://example.org/example");

    // Act and Assert
    assertFalse(extension.isCompatibleWith(new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith2() {
    // Arrange
    Extension extension = new Extension("Extension Name", "42", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertTrue(extension.isCompatibleWith(new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith3() {
    // Arrange
    Extension extension = new Extension("Extension Name", "", "Specification Vendor", "1.0.2", "Implementation Vendor",
        "42", "https://example.org/example");

    // Act and Assert
    assertFalse(extension.isCompatibleWith(new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith4() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "", "Implementation Vendor",
        "42", "https://example.org/example");

    // Act and Assert
    assertFalse(extension.isCompatibleWith(new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith5() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "Extension Name", "https://example.org/example");

    // Act and Assert
    assertFalse(extension.isCompatibleWith(new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith6() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertTrue(extension.isCompatibleWith(new Extension("Extension Name", "", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#isCompatibleWith(Extension)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Extension#isCompatibleWith(Extension)}
   */
  @Test
  public void testIsCompatibleWith_thenReturnTrue() {
    // Arrange
    Extension extension = new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example");

    // Act and Assert
    assertTrue(extension.isCompatibleWith(new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2",
        "Implementation Vendor", "42", "https://example.org/example")));
  }

  /**
   * Test {@link Extension#toString()}.
   * <p>
   * Method under test: {@link Extension#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals(
        "Extension-Name: Extension Name\n" + "Specification-Version: 1.0.2\n"
            + "Specification-Vendor: Specification Vendor\n" + "Implementation-Version: 1.0.2\n"
            + "Implementation-Vendor-Id: 42\n" + "Implementation-Vendor: Implementation Vendor\n"
            + "Implementation-URL: https://example.org/example\n",
        (new Extension("Extension Name", "1.0.2", "Specification Vendor", "1.0.2", "Implementation Vendor", "42",
            "https://example.org/example")).toString());
  }
}
