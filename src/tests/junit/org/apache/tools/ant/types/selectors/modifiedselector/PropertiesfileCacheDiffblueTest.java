package org.apache.tools.ant.types.selectors.modifiedselector;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class PropertiesfileCacheDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PropertiesfileCache#PropertiesfileCache()}
   *   <li>{@link PropertiesfileCache#setCachefile(File)}
   *   <li>{@link PropertiesfileCache#toString()}
   *   <li>{@link PropertiesfileCache#getCachefile()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    PropertiesfileCache actualPropertiesfileCache = new PropertiesfileCache();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualPropertiesfileCache.setCachefile(file);
    String actualToStringResult = actualPropertiesfileCache.toString();
    File actualCachefile = actualPropertiesfileCache.getCachefile();

    // Assert
    assertEquals(
        String.join("", "<PropertiesfileCache:cachefile=",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), ";noOfEntries=0>"),
        actualToStringResult);
    assertSame(file, actualCachefile);
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PropertiesfileCache#PropertiesfileCache(File)}
   *   <li>{@link PropertiesfileCache#setCachefile(File)}
   *   <li>{@link PropertiesfileCache#toString()}
   *   <li>{@link PropertiesfileCache#getCachefile()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange and Act
    PropertiesfileCache actualPropertiesfileCache = new PropertiesfileCache(
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualPropertiesfileCache.setCachefile(file);
    String actualToStringResult = actualPropertiesfileCache.toString();
    File actualCachefile = actualPropertiesfileCache.getCachefile();

    // Assert
    assertEquals(
        String.join("", "<PropertiesfileCache:cachefile=",
            Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), ";noOfEntries=0>"),
        actualToStringResult);
    assertSame(file, actualCachefile);
  }

  /**
   * Test {@link PropertiesfileCache#isValid()}.
   * <ul>
   *   <li>Given {@link PropertiesfileCache#PropertiesfileCache()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertiesfileCache#isValid()}
   */
  @Test
  public void testIsValid_givenPropertiesfileCache_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertiesfileCache()).isValid());
  }

  /**
   * Test {@link PropertiesfileCache#isValid()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertiesfileCache#isValid()}
   */
  @Test
  public void testIsValid_thenReturnTrue() {
    // Arrange
    PropertiesfileCache propertiesfileCache = new PropertiesfileCache();
    propertiesfileCache.setCachefile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertTrue(propertiesfileCache.isValid());
  }

  /**
   * Test {@link PropertiesfileCache#get(Object)}.
   * <p>
   * Method under test: {@link PropertiesfileCache#get(Object)}
   */
  @Test
  public void testGet() {
    // Arrange, Act and Assert
    assertNull(
        (new PropertiesfileCache(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).get("Key"));
  }

  /**
   * Test {@link PropertiesfileCache#get(Object)}.
   * <ul>
   *   <li>Given {@link PropertiesfileCache#PropertiesfileCache()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertiesfileCache#get(Object)}
   */
  @Test
  public void testGet_givenPropertiesfileCache() {
    // Arrange, Act and Assert
    assertNull((new PropertiesfileCache()).get("Key"));
  }

  /**
   * Test {@link PropertiesfileCache#iterator()}.
   * <p>
   * Method under test: {@link PropertiesfileCache#iterator()}
   */
  @Test
  public void testIterator() {
    // Arrange, Act and Assert
    assertFalse((new PropertiesfileCache()).iterator().hasNext());
  }
}
