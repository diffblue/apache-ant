package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import org.junit.Test;

public class ResourceLocationDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ResourceLocation}
   *   <li>{@link ResourceLocation#setBase(URL)}
   *   <li>{@link ResourceLocation#setLocation(String)}
   *   <li>{@link ResourceLocation#setPublicId(String)}
   *   <li>{@link ResourceLocation#getBase()}
   *   <li>{@link ResourceLocation#getLocation()}
   *   <li>{@link ResourceLocation#getPublicId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws MalformedURLException {
    // Arrange and Act
    ResourceLocation actualResourceLocation = new ResourceLocation();
    URL base = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL();
    actualResourceLocation.setBase(base);
    actualResourceLocation.setLocation("Location");
    actualResourceLocation.setPublicId("42");
    URL actualBase = actualResourceLocation.getBase();
    String actualLocation = actualResourceLocation.getLocation();

    // Assert
    assertEquals("42", actualResourceLocation.getPublicId());
    assertEquals("Location", actualLocation);
    String expectedToStringResult = String.join("", "file:",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString().concat(File.separator));
    assertEquals(expectedToStringResult, actualBase.toString());
    assertSame(base, actualBase);
  }
}
