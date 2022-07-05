package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CompositeMapperDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link CompositeMapper}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertTrue((new CompositeMapper()).getMappers().isEmpty());
  }

  /**
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName() {
    // Arrange, Act and Assert
    assertNull((new CompositeMapper()).mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName2() {
    // Arrange
    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new ChainedMapper());

    // Act
    String[] actualMapFileNameResult = compositeMapper.mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("foo.txt", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName3() {
    // Arrange
    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(null);

    // Act and Assert
    assertNull(compositeMapper.mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link CompositeMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName4() {
    // Arrange
    CompositeMapper compositeMapper = new CompositeMapper();
    compositeMapper.addConfigured(new CompositeMapper());

    // Act and Assert
    assertNull(compositeMapper.mapFileName("foo.txt"));
  }
}

