package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.text.ParseException;
import java.util.jar.Manifest;
import org.junit.Test;

public class SpecificationDiffblueTest {
  /**
   * Test {@link Specification#getSpecifications(Manifest)}.
   * <ul>
   *   <li>When {@link Manifest#Manifest()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getSpecifications(Manifest)}
   */
  @Test
  public void testGetSpecifications_whenManifest() throws ParseException {
    // Arrange, Act and Assert
    assertEquals(0, Specification.getSpecifications(new Manifest()).length);
  }

  /**
   * Test {@link Specification#getSpecifications(Manifest)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getSpecifications(Manifest)}
   */
  @Test
  public void testGetSpecifications_whenNull() throws ParseException {
    // Arrange, Act and Assert
    assertEquals(0, Specification.getSpecifications(null).length);
  }

  /**
   * Test {@link Specification#Specification(String, String, String, String, String, String)}.
   * <ul>
   *   <li>Then return SpecificationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#Specification(String, String, String, String, String, String)}
   */
  @Test
  public void testNewSpecification_thenReturnSpecificationVersionSizeIsThree() {
    // Arrange and Act
    Specification actualSpecification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Assert
    assertEquals("1.0.2", actualSpecification.getImplementationVersion());
    assertEquals("Dr", actualSpecification.getImplementationTitle());
    assertEquals("Dr", actualSpecification.getSpecificationTitle());
    assertEquals("Implementation Vendor", actualSpecification.getImplementationVendor());
    assertEquals("Specification Vendor", actualSpecification.getSpecificationVendor());
    assertNull(actualSpecification.getSections());
    assertEquals(3, actualSpecification.getSpecificationVersion().getSize());
  }

  /**
   * Test {@link Specification#Specification(String, String, String, String, String, String, String[])}.
   * <ul>
   *   <li>Then return SpecificationVersion Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#Specification(String, String, String, String, String, String, String[])}
   */
  @Test
  public void testNewSpecification_thenReturnSpecificationVersionSizeIsThree2() {
    // Arrange and Act
    Specification actualSpecification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor", new String[]{"Sections"});

    // Assert
    assertEquals("1.0.2", actualSpecification.getImplementationVersion());
    assertEquals("Dr", actualSpecification.getImplementationTitle());
    assertEquals("Dr", actualSpecification.getSpecificationTitle());
    assertEquals("Implementation Vendor", actualSpecification.getImplementationVendor());
    assertEquals("Specification Vendor", actualSpecification.getSpecificationVendor());
    assertEquals(3, actualSpecification.getSpecificationVersion().getSize());
    assertArrayEquals(new String[]{"Sections"}, actualSpecification.getSections());
  }

  /**
   * Test {@link Specification#Specification(String, String, String, String, String, String)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#Specification(String, String, String, String, String, String)}
   */
  @Test
  public void testNewSpecification_whenDot_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> new Specification("Dr", ".", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor"));

  }

  /**
   * Test {@link Specification#Specification(String, String, String, String, String, String, String[])}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#Specification(String, String, String, String, String, String, String[])}
   */
  @Test
  public void testNewSpecification_whenDot_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new Specification("Dr", ".", "Specification Vendor", "Dr",
        "1.0.2", "Implementation Vendor", new String[]{"Sections"}));

  }

  /**
   * Test {@link Specification#Specification(String, String, String, String, String, String, String[])}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Sections is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#Specification(String, String, String, String, String, String, String[])}
   */
  @Test
  public void testNewSpecification_whenNull_thenReturnSectionsIsNull() {
    // Arrange and Act
    Specification actualSpecification = new Specification("Dr", null, "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor", null);

    // Assert
    assertEquals("1.0.2", actualSpecification.getImplementationVersion());
    assertEquals("Dr", actualSpecification.getImplementationTitle());
    assertEquals("Dr", actualSpecification.getSpecificationTitle());
    assertEquals("Implementation Vendor", actualSpecification.getImplementationVendor());
    assertEquals("Specification Vendor", actualSpecification.getSpecificationVendor());
    assertNull(actualSpecification.getSections());
    assertNull(actualSpecification.getSpecificationVersion());
  }

  /**
   * Test {@link Specification#Specification(String, String, String, String, String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return SpecificationVersion is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#Specification(String, String, String, String, String, String)}
   */
  @Test
  public void testNewSpecification_whenNull_thenReturnSpecificationVersionIsNull() {
    // Arrange and Act
    Specification actualSpecification = new Specification("Dr", null, "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Assert
    assertEquals("1.0.2", actualSpecification.getImplementationVersion());
    assertEquals("Dr", actualSpecification.getImplementationTitle());
    assertEquals("Dr", actualSpecification.getSpecificationTitle());
    assertEquals("Implementation Vendor", actualSpecification.getImplementationVendor());
    assertEquals("Specification Vendor", actualSpecification.getSpecificationVendor());
    assertNull(actualSpecification.getSections());
    assertNull(actualSpecification.getSpecificationVersion());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Specification#getImplementationTitle()}
   *   <li>{@link Specification#getImplementationVendor()}
   *   <li>{@link Specification#getImplementationVersion()}
   *   <li>{@link Specification#getSpecificationTitle()}
   *   <li>{@link Specification#getSpecificationVendor()}
   *   <li>{@link Specification#getSpecificationVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    String actualImplementationTitle = specification.getImplementationTitle();
    String actualImplementationVendor = specification.getImplementationVendor();
    String actualImplementationVersion = specification.getImplementationVersion();
    String actualSpecificationTitle = specification.getSpecificationTitle();
    String actualSpecificationVendor = specification.getSpecificationVendor();

    // Assert
    assertEquals("1.0.2", actualImplementationVersion);
    assertEquals("Dr", actualImplementationTitle);
    assertEquals("Dr", actualSpecificationTitle);
    assertEquals("Implementation Vendor", actualImplementationVendor);
    assertEquals("Specification Vendor", actualSpecificationVendor);
    assertEquals(3, specification.getSpecificationVersion().getSize());
  }

  /**
   * Test {@link Specification#getSections()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Sections}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getSections()}
   */
  @Test
  public void testGetSections_thenReturnArrayOfStringWithSections() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Sections"}, (new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor", new String[]{"Sections"})).getSections());
  }

  /**
   * Test {@link Specification#getSections()}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getSections()}
   */
  @Test
  public void testGetSections_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor"))
        .getSections());
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");
    Specification other = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("COMPATIBLE", actualCompatibilityWith.toString());
    assertSame(other.COMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith2() {
    // Arrange
    Specification specification = new Specification("Dr", "42", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");
    Specification other = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("COMPATIBLE", actualCompatibilityWith.toString());
    assertSame(other.COMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith3() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");
    Specification other = new Specification("Dr", "", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("COMPATIBLE", actualCompatibilityWith.toString());
    assertSame(other.COMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <ul>
   *   <li>Then return toString is {@code INCOMPATIBLE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsIncompatible() {
    // Arrange
    Specification specification = new Specification("Mr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");
    Specification other = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("INCOMPATIBLE", actualCompatibilityWith.toString());
    assertSame(other.INCOMPATIBLE, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <ul>
   *   <li>Then return toString is {@code REQUIRE_IMPLEMENTATION_CHANGE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsRequireImplementationChange() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "Dr",
        "Implementation Vendor");
    Specification other = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("REQUIRE_IMPLEMENTATION_CHANGE", actualCompatibilityWith.toString());
    assertSame(other.REQUIRE_IMPLEMENTATION_CHANGE, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <ul>
   *   <li>Then return toString is {@code REQUIRE_SPECIFICATION_UPGRADE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsRequireSpecificationUpgrade() {
    // Arrange
    Specification specification = new Specification("Dr", "", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");
    Specification other = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("REQUIRE_SPECIFICATION_UPGRADE", actualCompatibilityWith.toString());
    assertSame(other.REQUIRE_SPECIFICATION_UPGRADE, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#getCompatibilityWith(Specification)}.
   * <ul>
   *   <li>Then return toString is {@code REQUIRE_VENDOR_SWITCH}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#getCompatibilityWith(Specification)}
   */
  @Test
  public void testGetCompatibilityWith_thenReturnToStringIsRequireVendorSwitch() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Dr");
    Specification other = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act
    Compatibility actualCompatibilityWith = specification.getCompatibilityWith(other);

    // Assert
    assertEquals("REQUIRE_VENDOR_SWITCH", actualCompatibilityWith.toString());
    assertSame(other.REQUIRE_VENDOR_SWITCH, actualCompatibilityWith);
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith() {
    // Arrange
    Specification specification = new Specification("Mr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act and Assert
    assertFalse(specification.isCompatibleWith(
        new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith2() {
    // Arrange
    Specification specification = new Specification("Dr", "42", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act and Assert
    assertTrue(specification.isCompatibleWith(
        new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith3() {
    // Arrange
    Specification specification = new Specification("Dr", "", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act and Assert
    assertFalse(specification.isCompatibleWith(
        new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith4() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "Dr",
        "Implementation Vendor");

    // Act and Assert
    assertFalse(specification.isCompatibleWith(
        new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith5() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Dr");

    // Act and Assert
    assertFalse(specification.isCompatibleWith(
        new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith6() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act and Assert
    assertTrue(specification
        .isCompatibleWith(new Specification("Dr", "", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#isCompatibleWith(Specification)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Specification#isCompatibleWith(Specification)}
   */
  @Test
  public void testIsCompatibleWith_thenReturnTrue() {
    // Arrange
    Specification specification = new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2",
        "Implementation Vendor");

    // Act and Assert
    assertTrue(specification.isCompatibleWith(
        new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")));
  }

  /**
   * Test {@link Specification#toString()}.
   * <p>
   * Method under test: {@link Specification#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals(
        "Specification-Title: Dr\n" + "Specification-Version: 1.0.2\n" + "Specification-Vendor: Specification Vendor\n"
            + "Implementation-Title: Dr\n" + "Implementation-Version: 1.0.2\n"
            + "Implementation-Vendor: Implementation Vendor\n",
        (new Specification("Dr", "1.0.2", "Specification Vendor", "Dr", "1.0.2", "Implementation Vendor")).toString());
  }
}
