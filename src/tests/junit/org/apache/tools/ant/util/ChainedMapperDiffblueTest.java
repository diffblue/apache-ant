package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ChainedMapperDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link ChainedMapper}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertTrue((new ChainedMapper()).getMappers().isEmpty());
  }

  /**
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName() {
    // Arrange and Act
    String[] actualMapFileNameResult = (new ChainedMapper()).mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("foo.txt", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName2() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new ChainedMapper());

    // Act
    String[] actualMapFileNameResult = chainedMapper.mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("foo.txt", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName3() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(null);

    // Act
    String[] actualMapFileNameResult = chainedMapper.mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("foo.txt", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName4() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new CompositeMapper());

    // Act and Assert
    assertNull(chainedMapper.mapFileName("foo.txt"));
  }
}

