package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;
import org.xml.sax.Locator;
import org.xml.sax.ext.Locator2Impl;

public class LocationDiffblueTest {
  /**
   * Test {@link Location#Location(Locator)}.
   * <ul>
   *   <li>Given {@code file:file:}.</li>
   *   <li>Then return FileName is {@code file:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(Locator)}
   */
  @Test
  public void testNewLocation_givenFileFile_thenReturnFileNameIsFile() {
    // Arrange
    Locator2Impl loc = new Locator2Impl();
    loc.setSystemId("file:file:");

    // Act
    Location actualLocation = new Location(loc);

    // Assert
    assertEquals("file:", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(Locator)}.
   * <ul>
   *   <li>Given {@code file:}.</li>
   *   <li>When {@link Locator2Impl#Locator2Impl()} SystemId is {@code file:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(Locator)}
   */
  @Test
  public void testNewLocation_givenFile_whenLocator2ImplSystemIdIsFile() {
    // Arrange
    Locator2Impl loc = new Locator2Impl();
    loc.setSystemId("file:");

    // Act
    Location actualLocation = new Location(loc);

    // Assert
    assertEquals("", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(Locator)}.
   * <ul>
   *   <li>Given {@code file:}.</li>
   *   <li>When {@link Locator2Impl#Locator2Impl(Locator)} with {@link Locator2Impl#Locator2Impl(Locator)} SystemId is {@code file:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(Locator)}
   */
  @Test
  public void testNewLocation_givenFile_whenLocator2ImplWithLocator2ImplSystemIdIsFile() {
    // Arrange
    Locator2Impl loc = new Locator2Impl(new Locator2Impl(new Locator2Impl()));
    loc.setSystemId("file:");

    // Act
    Location actualLocation = new Location(loc);

    // Assert
    assertEquals("", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(Locator)}.
   * <ul>
   *   <li>Given {@code file:%}.</li>
   *   <li>When {@link Locator2Impl#Locator2Impl(Locator)} with {@link Locator2Impl#Locator2Impl(Locator)} SystemId is {@code file:%}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(Locator)}
   */
  @Test
  public void testNewLocation_givenFile_whenLocator2ImplWithLocator2ImplSystemIdIsFile2() {
    // Arrange
    Locator2Impl loc = new Locator2Impl(new Locator2Impl(new Locator2Impl()));
    loc.setSystemId("file:%");

    // Act
    Location actualLocation = new Location(loc);

    // Assert
    assertEquals("", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(Locator)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link Locator2Impl#Locator2Impl()} SystemId is {@code foo}.</li>
   *   <li>Then return FileName is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(Locator)}
   */
  @Test
  public void testNewLocation_givenFoo_whenLocator2ImplSystemIdIsFoo_thenReturnFileNameIsFoo() {
    // Arrange
    Locator2Impl loc = new Locator2Impl();
    loc.setSystemId("foo");

    // Act
    Location actualLocation = new Location(loc);

    // Assert
    assertEquals("foo", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String)}.
   * <ul>
   *   <li>When {@code file:foo.txt}.</li>
   *   <li>Then return FileName is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String)}
   */
  @Test
  public void testNewLocation_whenFileFooTxt_thenReturnFileNameIsFooTxt() {
    // Arrange and Act
    Location actualLocation = new Location("file:foo.txt");

    // Assert
    assertEquals("foo.txt", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String)}.
   * <ul>
   *   <li>When {@code file:}.</li>
   *   <li>Then return FileName is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String)}
   */
  @Test
  public void testNewLocation_whenFile_thenReturnFileNameIsEmptyString() {
    // Arrange and Act
    Location actualLocation = new Location("file:");

    // Assert
    assertEquals("", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String, int, int)}.
   * <ul>
   *   <li>When {@code file:}.</li>
   *   <li>Then return FileName is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String, int, int)}
   */
  @Test
  public void testNewLocation_whenFile_thenReturnFileNameIsEmptyString2() {
    // Arrange and Act
    Location actualLocation = new Location("file:", 2, 10);

    // Assert
    assertEquals("", actualLocation.getFileName());
    assertEquals(10, actualLocation.getColumnNumber());
    assertEquals(2, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String, int, int)}.
   * <ul>
   *   <li>When {@code file:}.</li>
   *   <li>Then return LineNumber is ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String, int, int)}
   */
  @Test
  public void testNewLocation_whenFile_thenReturnLineNumberIsTen() {
    // Arrange and Act
    Location actualLocation = new Location("file:", 10, 10);

    // Assert
    assertEquals("", actualLocation.getFileName());
    assertEquals(10, actualLocation.getColumnNumber());
    assertEquals(10, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String)}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return FileName is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String)}
   */
  @Test
  public void testNewLocation_whenFooTxt_thenReturnFileNameIsFooTxt() {
    // Arrange and Act
    Location actualLocation = new Location("foo.txt");

    // Assert
    assertEquals("foo.txt", actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String, int, int)}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return FileName is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String, int, int)}
   */
  @Test
  public void testNewLocation_whenFooTxt_thenReturnFileNameIsFooTxt2() {
    // Arrange and Act
    Location actualLocation = new Location("foo.txt", 2, 10);

    // Assert
    assertEquals("foo.txt", actualLocation.getFileName());
    assertEquals(10, actualLocation.getColumnNumber());
    assertEquals(2, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(Locator)}.
   * <ul>
   *   <li>When {@link Locator2Impl#Locator2Impl()}.</li>
   *   <li>Then return FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(Locator)}
   */
  @Test
  public void testNewLocation_whenLocator2Impl_thenReturnFileNameIsNull() {
    // Arrange and Act
    Location actualLocation = new Location(new Locator2Impl());

    // Assert
    assertNull(actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String)}
   */
  @Test
  public void testNewLocation_whenNull_thenReturnFileNameIsNull() {
    // Arrange and Act
    Location actualLocation = new Location((String) null);

    // Assert
    assertNull(actualLocation.getFileName());
    assertEquals(0, actualLocation.getColumnNumber());
    assertEquals(0, actualLocation.getLineNumber());
  }

  /**
   * Test {@link Location#Location(String, int, int)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#Location(String, int, int)}
   */
  @Test
  public void testNewLocation_whenNull_thenReturnFileNameIsNull2() {
    // Arrange and Act
    Location actualLocation = new Location(null, 2, 10);

    // Assert
    assertNull(actualLocation.getFileName());
    assertEquals(10, actualLocation.getColumnNumber());
    assertEquals(2, actualLocation.getLineNumber());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Location#getColumnNumber()}
   *   <li>{@link Location#getFileName()}
   *   <li>{@link Location#getLineNumber()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Location location = new Location("foo.txt");

    // Act
    int actualColumnNumber = location.getColumnNumber();
    String actualFileName = location.getFileName();

    // Assert
    assertEquals("foo.txt", actualFileName);
    assertEquals(0, actualColumnNumber);
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Location#toString()}.
   * <ul>
   *   <li>Given {@link Location#Location(String)} with fileName is {@code foo.txt}.</li>
   *   <li>Then return {@code foo.txt:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#toString()}
   */
  @Test
  public void testToString_givenLocationWithFileNameIsFooTxt_thenReturnFooTxt() {
    // Arrange, Act and Assert
    assertEquals("foo.txt: ", (new Location("foo.txt")).toString());
  }

  /**
   * Test {@link Location#toString()}.
   * <ul>
   *   <li>Given {@link Location#UNKNOWN_LOCATION}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#toString()}
   */
  @Test
  public void testToString_givenUnknown_location_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Location.UNKNOWN_LOCATION.toString());
  }

  /**
   * Test {@link Location#toString()}.
   * <ul>
   *   <li>Then return {@code foo.txt:2:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#toString()}
   */
  @Test
  public void testToString_thenReturnFooTxt2() {
    // Arrange, Act and Assert
    assertEquals("foo.txt:2: ", (new Location("foo.txt", 2, 10)).toString());
  }

  /**
   * Test {@link Location#equals(Object)}, and {@link Location#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Location#equals(Object)}
   *   <li>{@link Location#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    Location location = Location.UNKNOWN_LOCATION;
    Location location2 = Location.UNKNOWN_LOCATION;

    // Act and Assert
    assertEquals(location, location2);
    int expectedHashCodeResult = location.hashCode();
    assertEquals(expectedHashCodeResult, location2.hashCode());
  }

  /**
   * Test {@link Location#equals(Object)}, and {@link Location#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Location#equals(Object)}
   *   <li>{@link Location#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual2() {
    // Arrange
    Location location = new Location(new Locator2Impl());
    Location location2 = Location.UNKNOWN_LOCATION;

    // Act and Assert
    assertEquals(location, location2);
    int expectedHashCodeResult = location.hashCode();
    assertEquals(expectedHashCodeResult, location2.hashCode());
  }

  /**
   * Test {@link Location#equals(Object)}, and {@link Location#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Location#equals(Object)}
   *   <li>{@link Location#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    Location location = Location.UNKNOWN_LOCATION;

    // Act and Assert
    assertEquals(location, location);
    int expectedHashCodeResult = location.hashCode();
    assertEquals(expectedHashCodeResult, location.hashCode());
  }

  /**
   * Test {@link Location#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Location("foo.txt"), Location.UNKNOWN_LOCATION);
  }

  /**
   * Test {@link Location#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange, Act and Assert
    assertNotEquals(new Location("foo.txt", 2, 10), Location.UNKNOWN_LOCATION);
  }

  /**
   * Test {@link Location#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(Location.UNKNOWN_LOCATION, null);
  }

  /**
   * Test {@link Location#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Location#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(Location.UNKNOWN_LOCATION, "Different type to Location");
  }
}
